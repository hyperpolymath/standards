// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

/**
 * Database layer for STAMP Telegram bot
 *
 * Uses SQLite for simplicity. Could be replaced with Postgres later.
 */

import { DB } from "https://deno.land/x/sqlite@v3.9.1/mod.ts";

// ============================================================================
// Types
// ============================================================================

export interface User {
  telegram_id: number;
  username: string | null;
  subscribed: boolean;
  consent_timestamp: number;
  consent_token: string;
  consent_proof: string;
  created_at: number;
  updated_at: number;
}

export interface Message {
  id: number;
  telegram_id: number;
  subject: string;
  body: string;
  sent_at: number;
  proof: string;
}

// ============================================================================
// Database Class
// ============================================================================

export class Database {
  private db: DB;

  constructor(path: string = "./db/stamp-bot.db") {
    this.db = new DB(path);
    this.init();
  }

  /**
   * Initialize database schema
   */
  private init() {
    // Users table
    this.db.execute(`
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
    `);

    // Messages table
    this.db.execute(`
      CREATE TABLE IF NOT EXISTS messages (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        telegram_id INTEGER NOT NULL,
        subject TEXT NOT NULL,
        body TEXT NOT NULL,
        sent_at INTEGER NOT NULL,
        proof TEXT NOT NULL,
        FOREIGN KEY (telegram_id) REFERENCES users(telegram_id)
      )
    `);

    // Index for faster lookups
    this.db.execute(`
      CREATE INDEX IF NOT EXISTS idx_messages_telegram_id
      ON messages(telegram_id)
    `);

    this.db.execute(`
      CREATE INDEX IF NOT EXISTS idx_messages_sent_at
      ON messages(sent_at)
    `);
  }

  /**
   * Subscribe a user
   */
  subscribeUser(
    telegram_id: number,
    username: string | null,
    consent_token: string,
    consent_proof: string,
  ): void {
    const now = Date.now();

    this.db.query(`
      INSERT INTO users (
        telegram_id, username, subscribed, consent_timestamp,
        consent_token, consent_proof, created_at, updated_at
      ) VALUES (?, ?, 1, ?, ?, ?, ?, ?)
      ON CONFLICT(telegram_id) DO UPDATE SET
        subscribed = 1,
        consent_timestamp = ?,
        consent_token = ?,
        consent_proof = ?,
        updated_at = ?
    `, [
      telegram_id,
      username,
      now,
      consent_token,
      consent_proof,
      now,
      now,
      now,
      consent_token,
      consent_proof,
      now,
    ]);
  }

  /**
   * Unsubscribe a user
   */
  unsubscribeUser(telegram_id: number): boolean {
    const result = this.db.query(`
      UPDATE users
      SET subscribed = 0, updated_at = ?
      WHERE telegram_id = ? AND subscribed = 1
    `, [Date.now(), telegram_id]);

    return result.length > 0;
  }

  /**
   * Get user by Telegram ID
   */
  getUser(telegram_id: number): User | null {
    const rows = this.db.query<[
      number,
      string | null,
      number,
      number,
      string,
      string,
      number,
      number,
    ]>(`
      SELECT telegram_id, username, subscribed, consent_timestamp,
             consent_token, consent_proof, created_at, updated_at
      FROM users
      WHERE telegram_id = ?
    `, [telegram_id]);

    if (rows.length === 0) return null;

    const row = rows[0];
    return {
      telegram_id: row[0],
      username: row[1],
      subscribed: row[2] === 1,
      consent_timestamp: row[3],
      consent_token: row[4],
      consent_proof: row[5],
      created_at: row[6],
      updated_at: row[7],
    };
  }

  /**
   * Check if user is subscribed
   */
  isSubscribed(telegram_id: number): boolean {
    const user = this.getUser(telegram_id);
    return user !== null && user.subscribed;
  }

  /**
   * Get all subscribed users
   */
  getSubscribedUsers(): User[] {
    const rows = this.db.query<[
      number,
      string | null,
      number,
      number,
      string,
      string,
      number,
      number,
    ]>(`
      SELECT telegram_id, username, subscribed, consent_timestamp,
             consent_token, consent_proof, created_at, updated_at
      FROM users
      WHERE subscribed = 1
    `);

    return rows.map(row => ({
      telegram_id: row[0],
      username: row[1],
      subscribed: row[2] === 1,
      consent_timestamp: row[3],
      consent_token: row[4],
      consent_proof: row[5],
      created_at: row[6],
      updated_at: row[7],
    }));
  }

  /**
   * Record a sent message
   */
  recordMessage(
    telegram_id: number,
    subject: string,
    body: string,
    proof: string,
  ): number {
    const result = this.db.query<[number]>(`
      INSERT INTO messages (telegram_id, subject, body, sent_at, proof)
      VALUES (?, ?, ?, ?, ?)
      RETURNING id
    `, [telegram_id, subject, body, Date.now(), proof]);

    return result[0][0];
  }

  /**
   * Get messages for a user
   */
  getUserMessages(telegram_id: number, limit: number = 10): Message[] {
    const rows = this.db.query<[number, number, string, string, number, string]>(`
      SELECT id, telegram_id, subject, body, sent_at, proof
      FROM messages
      WHERE telegram_id = ?
      ORDER BY sent_at DESC
      LIMIT ?
    `, [telegram_id, limit]);

    return rows.map(row => ({
      id: row[0],
      telegram_id: row[1],
      subject: row[2],
      body: row[3],
      sent_at: row[4],
      proof: row[5],
    }));
  }

  /**
   * Get the last message for a user
   */
  getLastMessage(telegram_id: number): Message | null {
    const messages = this.getUserMessages(telegram_id, 1);
    return messages.length > 0 ? messages[0] : null;
  }

  /**
   * Get statistics
   */
  getStats(): {
    total_users: number;
    subscribed_users: number;
    total_messages: number;
  } {
    const total_users = this.db.query<[number]>(`
      SELECT COUNT(*) FROM users
    `)[0][0];

    const subscribed_users = this.db.query<[number]>(`
      SELECT COUNT(*) FROM users WHERE subscribed = 1
    `)[0][0];

    const total_messages = this.db.query<[number]>(`
      SELECT COUNT(*) FROM messages
    `)[0][0];

    return { total_users, subscribed_users, total_messages };
  }

  /**
   * Close database connection
   */
  close() {
    this.db.close();
  }
}
