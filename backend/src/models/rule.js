const sqlite3 = require('sqlite3').verbose();
const db = new sqlite3.Database('./database.sqlite');
const logger = require('../utils/logger');

class Rule {
  static create(rule) {
	return new Promise((resolve, reject) => {
		try {
		  const tableName = 'rules'; // Your table name
		  const ruleStructure = Object.keys(rule); // Get the fields from the rule object

		  // Sanitize rule fields to remove unwanted characters from column names
		  const sanitizeField = (field) => field.replace(/[^\w]/g, '');

		  // Generate the SQL for creating the table dynamically
		  const tableFields = ruleStructure.map(field => `${sanitizeField(field)} TEXT`).join(", ");
		  const createTableSQL = `CREATE TABLE IF NOT EXISTS ${tableName} (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			${tableFields}
		  )`;

		  // Run the SQL to create the table if it doesn't exist
		  db.run(createTableSQL, [], function (err) {
			if (err) {
			  console.log('Error creating table:', err);
			  reject(err);
			} else {
			  console.log('Table checked/created successfully');

			  // Now prepare to insert data
			  const fields = ruleStructure.join(", ");
			  const placeholders = ruleStructure.map(() => '?').join(",");
			  const values = ruleStructure.map(k => rule[k]);

			  // Insert the data into the table
			  const insertSQL = `INSERT INTO ${tableName} (${fields}) VALUES (${placeholders})`;
			  console.log('Insert SQL:', insertSQL, 'Values:', values);

			  // Execute the insert statement
			  db.run(insertSQL, values, function (err) {
				if (err) {
				  console.log('Error creating rule:', err);
				  reject(err);
				} else {
				  resolve({ id: this.lastID, text: 'Rule successfully inserted' });
				}
			  });
			}
		  });
		} catch (ex) {
		  console.log("model/rule.js", ex.message);
		  reject(ex);
		}
	  });
  }

  static getAll(app) {

	const filter = app ? " WHERE app = '" + app + "'" : '';
	console.log("Final query", filter)
    return new Promise((resolve, reject) => {
      db.all('SELECT * FROM rules ' + filter + ' ORDER BY id', (err, rows) => {
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
