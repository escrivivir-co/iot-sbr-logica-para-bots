/**
 * Rule Model
 * 
 * SQLite-based persistence for Prolog rules.
 * Uses typed interfaces from the shared types module.
 */

import sqlite3 from 'sqlite3';
import type { Rule as RuleType, RuleInput, RuleCreatedResponse } from '../types';

// Simple logger for this module to avoid circular dependency issues
const log = {
  info: (msg: string, meta?: object) => console.log(`[INFO] ${msg}`, meta ? JSON.stringify(meta) : ''),
  error: (msg: string, meta?: object) => console.error(`[ERROR] ${msg}`, meta ? JSON.stringify(meta) : ''),
  warn: (msg: string, meta?: object) => console.warn(`[WARN] ${msg}`, meta ? JSON.stringify(meta) : ''),
};

// Lazy database initialization
let db: sqlite3.Database | null = null;

function getDb(): sqlite3.Database {
  if (!db) {
    db = new sqlite3.Database('./database.sqlite');
  }
  return db;
}

/**
 * Initialize the rules table
 */
export function initRulesTable(): Promise<void> {
  
  return new Promise((resolve, reject) => {
    const createTableSQL = `
      CREATE TABLE IF NOT EXISTS rules (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        content TEXT NOT NULL,
        app TEXT,
        predicate TEXT,
        arity TEXT,
        example TEXT,
        evalCompatible TEXT
      )
    `;
    
    getDb().run(createTableSQL, (err) => {
      if (err) {
        log.error('Error creating rules table', { error: err.message });
        reject(err);
      } else {
        log.info('Rules table initialized');
        resolve();
      }
    });
  });
}

/**
 * Create a new rule
 */
export function createRule(rule: RuleInput): Promise<RuleCreatedResponse> {
  
  return new Promise((resolve, reject) => {
    // Get fields that have values
    const fields = Object.keys(rule).filter(k => rule[k as keyof RuleInput] !== undefined);
    const placeholders = fields.map(() => '?').join(', ');
    const values = fields.map(k => rule[k as keyof RuleInput]);
    
    const insertSQL = `INSERT INTO rules (${fields.join(', ')}) VALUES (${placeholders})`;
    
    getDb().run(insertSQL, values, function(err) {
      if (err) {
        log.error('Error creating rule', { error: err.message, rule });
        reject(err);
      } else {
        resolve({
          id: this.lastID,
          text: 'Rule successfully inserted',
        });
      }
    });
  });
}

/**
 * Get all rules, optionally filtered by app
 */
export function getRules(app?: string): Promise<RuleType[]> {
  
  return new Promise((resolve, reject) => {
    const filter = app ? ` WHERE app = ?` : '';
    const params = app ? [app] : [];
    
    getDb().all(`SELECT * FROM rules${filter} ORDER BY id`, params, (err, rows) => {
      if (err) {
        log.error('Error getting rules', { error: err.message, app });
        reject(err);
      } else {
        resolve(rows as RuleType[]);
      }
    });
  });
}

/**
 * Get a single rule by ID
 */
export function getRuleById(id: number): Promise<RuleType | null> {
  
  return new Promise((resolve, reject) => {
    getDb().get('SELECT * FROM rules WHERE id = ?', [id], (err, row) => {
      if (err) {
        log.error('Error getting rule', { error: err.message, id });
        reject(err);
      } else {
        resolve(row as RuleType | null);
      }
    });
  });
}

/**
 * Update a rule
 */
export function updateRule(id: number, rule: Partial<RuleInput>): Promise<boolean> {
  
  return new Promise((resolve, reject) => {
    const fields = Object.keys(rule).filter(k => rule[k as keyof RuleInput] !== undefined);
    const setClause = fields.map(f => `${f} = ?`).join(', ');
    const values = [...fields.map(k => rule[k as keyof RuleInput]), id];
    
    getDb().run(`UPDATE rules SET ${setClause} WHERE id = ?`, values, function(err) {
      if (err) {
        log.error('Error updating rule', { error: err.message, id, rule });
        reject(err);
      } else {
        resolve(this.changes > 0);
      }
    });
  });
}

/**
 * Delete a rule
 */
export function deleteRule(id: number): Promise<void> {
  
  return new Promise((resolve, reject) => {
    getDb().run('DELETE FROM rules WHERE id = ?', [id], function(err) {
      if (err) {
        log.error('Error deleting rule', { error: err.message, id });
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

/**
 * Close database connection
 */
export function closeDatabase(): Promise<void> {
  return new Promise((resolve, reject) => {
    if (!db) {
      resolve();
      return;
    }
    db.close((err) => {
      if (err) {
        reject(err);
      } else {
        db = null;
        resolve();
      }
    });
  });
}

// Legacy export for compatibility
export const Rule = {
  create: createRule,
  getAll: getRules,
  getById: getRuleById,
  update: updateRule,
  delete: deleteRule,
};

export default Rule;
