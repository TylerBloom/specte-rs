use axum::{
    Json, Router,
    extract::{FromRequest, Path, Request, State},
    http::StatusCode,
    response::IntoResponse,
    routing::*,
};
use base64::{Engine, prelude::BASE64_STANDARD};
use chrono::{DateTime, Utc};
use clap::Parser;
use futures::StreamExt;
use serde_json::{Value, to_value};
use sqlx::{PgPool, postgres::PgPoolOptions};
use uuid::Uuid;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, env = "DB_URL")]
    db_url: String,
}

#[tokio::main]
async fn main() {
    let Args { db_url } = Args::parse();

    let pool = create_conn_pool(db_url).await;
    let state = ServerState { pool };

    let app = Router::new()
        .route("/user", post(create_user))
        .route("/user/{user_id}", get(get_user))
        .route("/user/{user_id}/roms/list", get(list_user_roms))
        .route("/user/{user_id}/roms", get(get_user_roms))
        .route("/user/{user_id}/roms/{rom_id}", get(get_rom))
        .route("/user/{user_id}/roms", post(add_rom))
        .route("/user/{user_id}/roms/{rom_id}", post(update_rom))
        .route("/purge", delete(purge_db))
        .with_state(state);

    println!("Setup complete!!");

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    println!("Listening on port 3000...");
    axum::serve(listener, app).await.unwrap();
}

async fn create_conn_pool(url: String) -> PgPool {
    let pool = PgPoolOptions::new()
        .max_connections(10)
        .connect(&url)
        .await
        .unwrap();

    sqlx::query(
        r#"
CREATE TABLE IF NOT EXISTS users (
  user_id uuid,
  created_at timestamptz,
  last_updated timestamptz,
  name text
);
"#,
    )
    .execute(&pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
CREATE TABLE IF NOT EXISTS roms (
  rom_id uuid,
  user_id uuid,
  created_at timestamptz,
  last_updated timestamptz,
  rom bytea
);
"#,
    )
    .execute(&pool)
    .await
    .unwrap();

    pool
}

#[derive(Debug, Clone)]
struct ServerState {
    pool: PgPool,
}

// struct UserId(pub Uuid);
// struct RomId(pub Uuid);
struct Rom(pub Vec<u8>);

impl<S: Sync> FromRequest<S> for Rom {
    type Rejection = (StatusCode, String);

    async fn from_request(req: Request, _state: &S) -> Result<Self, Self::Rejection> {
        let mut data = Vec::new();
        let mut stream = req.into_body().into_data_stream();
        while let Some(Ok(bytes)) = stream.next().await {
            data.extend_from_slice(&bytes);
        }
        match String::from_utf8(data) {
            Ok(data) => match BASE64_STANDARD.decode(data.trim()) {
                // TODO: This needs to be checked for validity
                Ok(data) => Ok(Self(data)),
                Err(err) => Err((StatusCode::BAD_REQUEST, err.to_string())),
            },
            Err(err) => Err((StatusCode::BAD_REQUEST, err.to_string())),
        }
    }
}

/// Add user to the user table
async fn create_user(State(state): State<ServerState>) -> (StatusCode, Json<Value>) {
    let id = Uuid::new_v4();
    let now = Utc::now();
    let query = sqlx::query_as(
        "
INSERT INTO
    users ( user_id, created_at, last_updated, name )
VALUES
    ( $1, $2, $3, $4 )
RETURNING
    user_id, created_at, last_updated, name
",
    )
    .bind(id)
    .bind(now)
    .bind(now)
    .bind(String::new());
    let row: (Uuid, DateTime<Utc>, DateTime<Utc>, String) =
        match query.persistent(true).fetch_one(&state.pool).await {
            Ok(row) => row,
            Err(err) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(Value::String(err.to_string())),
                );
            }
        };
    let data = serde_json::json!({
        "id": row.0,
        "created_at": row.1,
        "last_udpated": row.2,
        "name": row.3
    });
    (StatusCode::CREATED, Json(data))
}

/// Get user metadata
async fn get_user(
    State(state): State<ServerState>,
    Path(id): Path<Uuid>,
) -> (StatusCode, Json<Value>) {
    let query = sqlx::query_as(
        "
SELECT
    user_id, created_at, last_updated, name
FROM
    users
WHERE
    user_id = $1
",
    )
    .bind(id);
    let row: (Uuid, DateTime<Utc>, DateTime<Utc>, String) =
        match query.persistent(true).fetch_one(&state.pool).await {
            Ok(row) => row,
            Err(err) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(Value::String(err.to_string())),
                );
            }
        };

    let data = serde_json::json!({
        "id": row.0,
        "created_at": row.1,
        "last_updated": row.2,
        "name": row.3,
    });
    (StatusCode::OK, Json(data))
}

/// Get the metadata for all ROM from the ROMs table that belong to the user
async fn list_user_roms(
    State(state): State<ServerState>,
    Path(id): Path<Uuid>,
) -> (StatusCode, Json<Value>) {
    let query = sqlx::query_as(
        "
SELECT
    rom_id, user_id, created_at, last_updated
FROM
    roms
WHERE
    user_id = $1
",
    )
    .bind(id);

    let data: Vec<(Uuid, Uuid, DateTime<Utc>, DateTime<Utc>)> =
        match query.persistent(true).fetch_all(&state.pool).await {
            Ok(row) => row,
            Err(err) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(Value::String(err.to_string())),
                );
            }
        };

    let data = data.into_iter().map(|row|{
        serde_json::json!({
            "rom_id": row.0,
            "user_id": row.1,
            "created_at": row.2,
            "last_updated": row.3
        })
    }).collect();

    (StatusCode::OK, Json(Value::Array(data)))
}

/// Get all the ROM and metadata from the ROMs tables that belong to the user
async fn get_user_roms(
    State(state): State<ServerState>,
    Path(id): Path<Uuid>,
) -> impl IntoResponse {
    #[allow(clippy::type_complexity)]
    let row: Vec<(Uuid, Uuid, DateTime<Utc>, DateTime<Utc>, Vec<u8>)> = sqlx::query_as(
        "
SELECT
    rom_id, user_id, created_at, last_updated, rom
FROM
    roms
WHERE
    user_id = $1
",
    )
    .bind(id)
    .persistent(true)
    .fetch_all(&state.pool)
    .await
    .unwrap();

    let data = row
        .into_iter()
        .map(|(rom_id, user_id, created, updated, rom)| {
            serde_json::json!({
                "rom_id": rom_id,
                "user_id": user_id,
                "created_at": created,
                "last_updated": updated,
                "rom": BASE64_STANDARD.encode(rom)
            })
        })
        .collect();

    (StatusCode::OK, Json(Value::Array(data)))
}

/// Get the ROM and metadata from the ROMs tables
async fn get_rom(
    State(state): State<ServerState>,
    Path((user_id, rom_id)): Path<(Uuid, Uuid)>,
) -> (StatusCode, Json<Value>) {
    #[allow(clippy::type_complexity)]
    let row: (Uuid, Uuid, DateTime<Utc>, DateTime<Utc>, Vec<u8>) = sqlx::query_as(
        "
SELECT
    rom_id, user_id, created_at, last_updated, rom
FROM
    roms
WHERE
    user_id = $1
    AND rom_id = $2
",
    )
    .bind(user_id)
    .bind(rom_id)
    .persistent(true)
    .fetch_one(&state.pool)
    .await
    .unwrap();

    let data = serde_json::json!({
        "rom_id": row.0,
        "user_id": row.1,
        "created_at": row.2,
        "last_updated": row.3,
        "rom": BASE64_STANDARD.encode(row.4)
    });

    (StatusCode::OK, Json(data))
}

/// Adds a ROM to the ROMs table and marks it as belonging to the user
async fn add_rom(
    State(state): State<ServerState>,
    Path(id): Path<Uuid>,
    Rom(rom): Rom,
) -> (StatusCode, Json<Value>) {
    let rom_id = Uuid::new_v4();
    let now = Utc::now();
    let row: (Uuid, Uuid, DateTime<Utc>, DateTime<Utc>) = sqlx::query_as(
        r#"
INSERT INTO
    roms (rom_id, user_id, created_at, last_updated, rom)
VALUES
    ($1, $2, $3, $4, $5)
RETURNING
    rom_id, user_id, created_at, last_updated
"#,
    )
    .bind(rom_id)
    .bind(id)
    .bind(now)
    .bind(now)
    .bind(rom)
    .persistent(true)
    .fetch_one(&state.pool)
    .await
    .unwrap();
    let data = serde_json::json!({
        "rom_id": row.0,
        "user_id": row.1,
        "created_at": row.2,
        "last_udpated": row.3
    });
    (StatusCode::CREATED, Json(data))
}

/// Overwrites the ROM with the new ROM data
async fn update_rom(
    State(state): State<ServerState>,
    Path((user_id, rom_id)): Path<(Uuid, Uuid)>,
    Rom(rom): Rom,
) -> (StatusCode, Json<Value>) {
    let row: (DateTime<Utc>,) = sqlx::query_as(
        "
UPDATE
    roms
SET
    rom = $1, last_updated = $2
WHERE
    user_id = $3
    AND rom_id = $4
RETURNING
    last_updated
",
    )
    .bind(rom)
    .bind(Utc::now())
    .bind(user_id)
    .bind(rom_id)
    .persistent(true)
    .fetch_one(&state.pool)
    .await
    .unwrap();

    (StatusCode::OK, Json(to_value(row.0).unwrap()))
}

async fn purge_db(State(state): State<ServerState>) -> (StatusCode, Json<Value>) {
    let _row: Vec<(i32,)> = sqlx::query_as("DELETE FROM users WHERE true")
        .persistent(true)
        .fetch_all(&state.pool)
        .await
        .unwrap();
    let _row: Vec<(i32,)> = sqlx::query_as("DELETE FROM roms WHERE true")
        .persistent(true)
        .fetch_all(&state.pool)
        .await
        .unwrap();
    (StatusCode::OK, Json(Value::Null))
}
