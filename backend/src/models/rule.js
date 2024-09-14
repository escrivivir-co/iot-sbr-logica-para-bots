const sqlite3 = require('sqlite3').verbose();
const db = new sqlite3.Database('./database.sqlite');
const logger = require('../utils/logger');

class Rule {
  static create(text) {
    return new Promise((resolve, reject) => {
      db.run('INSERT INTO rules (text) VALUES (?)', [text], function(err) {
        if (err) {
          logger.error('Error creating rule:', err);
          reject(err);
        } else {
          resolve({ id: this.lastID, text });
        }
      });
    });
  }

  static getAll() {
    return new Promise((resolve, reject) => {
      db.all('SELECT * FROM rules ORDER BY id', (err, rows) => {
        if (err) {
          logger.error('Error getting rules:', err);
          reject(err);
        } else {
          resolve(rows);
        }
      });
    });
  }

  static delete(id) {
    return new Promise((resolve, reject) => {
      db.run('DELETE FROM rules WHERE id = ?', [id], function(err) {
        if (err) {
          logger.error('Error deleting rule:', err);
          reject(err);
        } else {
          resolve();
        }
      });
    });
  }
}

module.exports = Rule;
