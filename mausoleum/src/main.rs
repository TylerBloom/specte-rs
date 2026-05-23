use axum::Json;
use axum::Router;
use axum::extract::FromRequest;
use axum::extract::Path;
use axum::extract::Request;
use axum::extract::State;
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::*;
use base64::Engine;
use base64::prelude::BASE64_STANDARD;
use chrono::DateTime;
use chrono::Utc;
use clap::Parser;
use futures::StreamExt;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_json::to_value;
use sqlx::FromRow;
use sqlx::PgPool;
use sqlx::postgres::PgPoolOptions;
use sqlx::postgres::PgRow;
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

// struct UserId(pub Uuid);
// struct RomId(pub Uuid);

