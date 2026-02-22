// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Database layer for STAMP Telegram bot - SQLite

open Sqlite

type user = {
  telegram_id: int,
  username: option<string>,
  subscribed: bool,
  consent_timestamp: float,
  consent_token: string,
  consent_proof: string,
  created_at: float,
  updated_at: float,
}

type message = {
  id: int,
  telegram_id: int,
  subject: string,
  body: string,
  sent_at: float,
  proof: string,
}

type stats = {
  total_users: int,
  subscribed_users: int,
  total_messages: int,
}

type t = {db: Sqlite.db}

let make = (~path: string="./db/stamp-bot.db"): t => {
  let db = makeDB(path)

  // Users table
  db->execute(`
    CREATE TABLE IF NOT EXISTS users (
      telegram_id INTEGER PRIMARY KEY,
      username TEXT,
      subscribed BOOLEAN NOT NULL DEFAULT 1,
      consent_timestamp INTEGER NOT NULL,
      consent_token TEXT NOT NULL,
      consent_proof TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      updated_at INTEGER NOT NULL
    )
  `)

  // Messages table
  db->execute(`
    CREATE TABLE IF NOT EXISTS messages (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      telegram_id INTEGER NOT NULL,
      subject TEXT NOT NULL,
      body TEXT NOT NULL,
      sent_at INTEGER NOT NULL,
      proof TEXT NOT NULL,
      FOREIGN KEY (telegram_id) REFERENCES users(telegram_id)
    )
  `)

  // Indexes
  db->execute(`
    CREATE INDEX IF NOT EXISTS idx_messages_telegram_id
    ON messages(telegram_id)
  `)

  db->execute(`
    CREATE INDEX IF NOT EXISTS idx_messages_sent_at
    ON messages(sent_at)
  `)

  {db: db}
}

let subscribeUser = (t: t, ~telegramId: int, ~username: option<string>, ~consentToken: string, ~consentProof: string) => {
  let now = Date.now()
  let _ = t.db->query(
    `INSERT INTO users (
      telegram_id, username, subscribed, consent_timestamp,
      consent_token, consent_proof, created_at, updated_at
    ) VALUES (?, ?, 1, ?, ?, ?, ?, ?)
    ON CONFLICT(telegram_id) DO UPDATE SET
      subscribed = 1,
      consent_timestamp = ?,
      consent_token = ?,
      consent_proof = ?,
      updated_at = ?`,
    [
      JSON.Encode.int(telegramId),
      username->Option.mapOr(JSON.Encode.null, JSON.Encode.string),
      JSON.Encode.float(now),
      JSON.Encode.string(consentToken),
      JSON.Encode.string(consentProof),
      JSON.Encode.float(now),
      JSON.Encode.float(now),
      JSON.Encode.float(now),
      JSON.Encode.string(consentToken),
      JSON.Encode.string(consentProof),
      JSON.Encode.float(now),
    ],
  )
}

let unsubscribeUser = (t: t, ~telegramId: int): bool => {
  let result = t.db->query(
    `UPDATE users SET subscribed = 0, updated_at = ? WHERE telegram_id = ? AND subscribed = 1`,
    [JSON.Encode.float(Date.now()), JSON.Encode.int(telegramId)],
  )
  Array.length(result) > 0
}

let getUser = (t: t, ~telegramId: int): option<user> => {
  let rows = t.db->query(
    `SELECT telegram_id, username, subscribed, consent_timestamp,
            consent_token, consent_proof, created_at, updated_at
     FROM users WHERE telegram_id = ?`,
    [JSON.Encode.int(telegramId)],
  )

  switch rows[0] {
  | Some(row) =>
    Some({
      telegram_id: row[0]->Option.flatMap(JSON.Decode.float)->Option.mapOr(0, Float.toInt),
      username: row[1]->Option.flatMap(JSON.Decode.string),
      subscribed: row[2]->Option.flatMap(JSON.Decode.float)->Option.mapOr(false, v => v == 1.0),
      consent_timestamp: row[3]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
      consent_token: row[4]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
      consent_proof: row[5]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
      created_at: row[6]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
      updated_at: row[7]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
    })
  | None => None
  }
}

let isSubscribed = (t: t, ~telegramId: int): bool => {
  switch getUser(t, ~telegramId) {
  | Some(user) => user.subscribed
  | None => false
  }
}

let getSubscribedUsers = (t: t): array<user> => {
  let rows = t.db->query(
    `SELECT telegram_id, username, subscribed, consent_timestamp,
            consent_token, consent_proof, created_at, updated_at
     FROM users WHERE subscribed = 1`,
    [],
  )

  rows->Array.map(row => {
    telegram_id: row[0]->Option.flatMap(JSON.Decode.float)->Option.mapOr(0, Float.toInt),
    username: row[1]->Option.flatMap(JSON.Decode.string),
    subscribed: true,
    consent_timestamp: row[3]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
    consent_token: row[4]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
    consent_proof: row[5]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
    created_at: row[6]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
    updated_at: row[7]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
  })
}

let recordMessage = (t: t, ~telegramId: int, ~subject: string, ~body: string, ~proof: string): int => {
  let result = t.db->query(
    `INSERT INTO messages (telegram_id, subject, body, sent_at, proof)
     VALUES (?, ?, ?, ?, ?) RETURNING id`,
    [
      JSON.Encode.int(telegramId),
      JSON.Encode.string(subject),
      JSON.Encode.string(body),
      JSON.Encode.float(Date.now()),
      JSON.Encode.string(proof),
    ],
  )

  switch result[0] {
  | Some(row) => row[0]->Option.flatMap(JSON.Decode.float)->Option.mapOr(0, Float.toInt)
  | None => 0
  }
}

let getUserMessages = (t: t, ~telegramId: int, ~limit: int=10): array<message> => {
  let rows = t.db->query(
    `SELECT id, telegram_id, subject, body, sent_at, proof
     FROM messages WHERE telegram_id = ?
     ORDER BY sent_at DESC LIMIT ?`,
    [JSON.Encode.int(telegramId), JSON.Encode.int(limit)],
  )

  rows->Array.map(row => {
    id: row[0]->Option.flatMap(JSON.Decode.float)->Option.mapOr(0, Float.toInt),
    telegram_id: row[1]->Option.flatMap(JSON.Decode.float)->Option.mapOr(0, Float.toInt),
    subject: row[2]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
    body: row[3]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
    sent_at: row[4]->Option.flatMap(JSON.Decode.float)->Option.getOr(0.0),
    proof: row[5]->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
  })
}

let getLastMessage = (t: t, ~telegramId: int): option<message> => {
  let messages = getUserMessages(t, ~telegramId, ~limit=1)
  messages[0]
}

let getStats = (t: t): stats => {
  let totalUsers =
    t.db
    ->query(`SELECT COUNT(*) FROM users`, [])
    ->Array.get(0)
    ->Option.flatMap(row => row[0])
    ->Option.flatMap(JSON.Decode.float)
    ->Option.mapOr(0, Float.toInt)

  let subscribedUsers =
    t.db
    ->query(`SELECT COUNT(*) FROM users WHERE subscribed = 1`, [])
    ->Array.get(0)
    ->Option.flatMap(row => row[0])
    ->Option.flatMap(JSON.Decode.float)
    ->Option.mapOr(0, Float.toInt)

  let totalMessages =
    t.db
    ->query(`SELECT COUNT(*) FROM messages`, [])
    ->Array.get(0)
    ->Option.flatMap(row => row[0])
    ->Option.flatMap(JSON.Decode.float)
    ->Option.mapOr(0, Float.toInt)

  {total_users: totalUsers, subscribed_users: subscribedUsers, total_messages: totalMessages}
}

let close = (t: t) => t.db->Sqlite.close
