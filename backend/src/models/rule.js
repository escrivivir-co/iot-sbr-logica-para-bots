const db = require('../config/database');

class Rule {
  static async getAll() {
    const result = await db.query('SELECT * FROM rules');
    return result.rows;
  }

  static async create(name, content) {
    const result = await db.query(
      'INSERT INTO rules (name, content) VALUES ($1, $2) RETURNING *',
      [name, content]
    );
    return result.rows[0];
  }

  static async update(id, name, content) {
    const result = await db.query(
      'UPDATE rules SET name = $1, content = $2 WHERE id = $3 RETURNING *',
      [name, content, id]
    );
    return result.rows[0];
  }

  static async delete(id) {
    await db.query('DELETE FROM rules WHERE id = $1', [id]);
  }
}

module.exports = Rule;
