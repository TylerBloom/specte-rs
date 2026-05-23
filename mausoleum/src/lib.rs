use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[cfg(feature = "server")]
pub use server::*;

/* --------- Wrapper types for all API endpoints --------- */

impl _ {
    pub static ENDPOINT: &str = "";
}

impl _ {
    pub async fn process() -> Result<JsonResponse<Self>, anyhow::Errro> {
        todo!()
    }
}

pub struct Rom(pub Vec<u8>);

#[derive(Debug, Serialize, Deserialize)]
pub struct RomMetadata {
    rom_id: Uuid,
    user_id: Uuid,
    created_at: DateTime<Utc>,
    last_updated: DateTime<Utc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RomResponse(RomMetadata, String);

#[derive(Debug, Serialize, Deserialize)]
pub struct UserMetadata {
    user_id: Uuid,
    created_at: DateTime<Utc>,
    last_updated: DateTime<Utc>,
    name: String,
}

#[cfg(feature = "server")]
mod server {
    use axum::{Json, extract::{FromRequest, Path, Request, State}, http::StatusCode, response::IntoResponse};
    use base64::{Engine, prelude::BASE64_STANDARD};
    use chrono::{DateTime, Utc};
    use futures::StreamExt;
    use serde::Serialize;
    use serde_json::{Value, to_value};
    use sqlx::{FromRow, PgPool, postgres::PgRow};
    use uuid::Uuid;

    use crate::{Rom, RomMetadata, RomResponse, UserMetadata};


    #[derive(Debug, Clone)]
    pub struct ServerState {
        pool: PgPool,
    }

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

    pub struct JsonResponse<T = Value>(StatusCode, Json<T>);

    impl<T: Serialize> IntoResponse for JsonResponse<T> {
        fn into_response(self) -> axum::response::Response {
            let Self(status, json) = self;
            (status, json).into_response()
        }
    }

    impl<T> From<(StatusCode, T)> for JsonResponse<T> {
        fn from((status, data): (StatusCode, T)) -> Self {
            Self(status, Json(data))
        }
    }

    impl<T> From<T> for JsonResponse<T> {
        fn from(value: T) -> Self {
            Self(StatusCode::OK, Json(value))
        }
    }

    impl sqlx::FromRow<'_, PgRow> for UserMetadata {
        fn from_row(row: &'_ PgRow) -> Result<Self, sqlx::Error> {
            let (user_id, created_at, last_updated, name) =
                <(Uuid, DateTime<Utc>, DateTime<Utc>, String) as FromRow<'_, _>>::from_row(row)?;
            Ok(Self {
                user_id,
                created_at,
                last_updated,
                name,
            })
        }
    }

    /// Add user to the user table
    pub async fn create_user(State(state): State<ServerState>) -> JsonResponse<Value> {
        let id = Uuid::new_v4();
        let now = Utc::now();
        let query = sqlx::query_as::<_, UserMetadata>(
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

        match query.persistent(true).fetch_one(&state.pool).await {
            Ok(data) => (StatusCode::CREATED, to_value(data).unwrap()),
            Err(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                Value::String(err.to_string()),
            ),
        }
        .into()
    }

    /// Get user metadata
    pub async fn get_user(State(state): State<ServerState>, Path(id): Path<Uuid>) -> JsonResponse {
        let query = sqlx::query_as::<_, UserMetadata>(
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

        match query.persistent(true).fetch_one(&state.pool).await {
            Ok(row) => (StatusCode::OK, to_value(row).unwrap()),
            Err(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                Value::String(err.to_string()),
            ),
        }
        .into()
    }

    /// Get the metadata for all ROM from the ROMs table that belong to the user
    pub async fn list_user_roms(
        State(state): State<ServerState>,
        Path(id): Path<Uuid>,
    ) -> JsonResponse {
        let query = sqlx::query_as::<_, RomMetadata>(
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

        match query.persistent(true).fetch_all(&state.pool).await {
            Ok(row) => (StatusCode::OK, to_value(row).unwrap()),
            Err(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                Value::String(err.to_string()),
            ),
        }
        .into()
    }

    /// Get all the ROM and metadata from the ROMs tables that belong to the user
    pub async fn get_user_roms(
        State(state): State<ServerState>,
        Path(id): Path<Uuid>,
    ) -> JsonResponse<Vec<RomResponse>> {
        let data: Vec<RomResponse> = sqlx::query_as(
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

        data.into()
    }

    impl sqlx::FromRow<'_, PgRow> for RomResponse {
        fn from_row(row: &'_ PgRow) -> Result<Self, sqlx::Error> {
            let (rom_id, user_id, created_at, last_updated, rom) =
                <(Uuid, Uuid, DateTime<Utc>, DateTime<Utc>, Vec<u8>) as FromRow<'_, _>>::from_row(
                    row,
                )?;
            let meta = RomMetadata {
                rom_id,
                user_id,
                created_at,
                last_updated,
            };
            Ok(Self(meta, BASE64_STANDARD.encode(rom)))
        }
    }

    /// Get the ROM and metadata from the ROMs tables
    pub async fn get_rom(
        State(state): State<ServerState>,
        Path((user_id, rom_id)): Path<(Uuid, Uuid)>,
    ) -> JsonResponse<RomResponse> {
        #[allow(clippy::type_complexity)]
        let data: RomResponse = sqlx::query_as(
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

        data.into()
    }

    impl sqlx::FromRow<'_, PgRow> for RomMetadata {
        fn from_row(row: &'_ PgRow) -> Result<Self, sqlx::Error> {
            let (rom_id, user_id, created_at, last_updated) =
                <(Uuid, Uuid, DateTime<Utc>, DateTime<Utc>) as FromRow<'_, _>>::from_row(row)?;
            Ok(Self {
                rom_id,
                user_id,
                created_at,
                last_updated,
            })
        }
    }

    /// Adds a ROM to the ROMs table and marks it as belonging to the user
    pub async fn add_rom(
        State(state): State<ServerState>,
        Path(id): Path<Uuid>,
        Rom(rom): Rom,
    ) -> JsonResponse<RomMetadata> {
        let rom_id = Uuid::new_v4();
        let now = Utc::now();
        let data: RomMetadata = sqlx::query_as(
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
        (StatusCode::CREATED, data).into()
    }

    /// Overwrites the ROM with the new ROM data
    pub async fn update_rom(
        State(state): State<ServerState>,
        Path((user_id, rom_id)): Path<(Uuid, Uuid)>,
        Rom(rom): Rom,
    ) -> Json<DateTime<Utc>> {
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

        row.0.into()
    }

    pub async fn purge_db(State(state): State<ServerState>) -> JsonResponse<()> {
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
        ().into()
    }
}
