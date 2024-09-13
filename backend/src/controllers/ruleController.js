const db = require('../config/database');
const prologService = require('../services/prologService');

exports.getRules = async (req, res) => {
  try {
    const result = await db.query('SELECT * FROM rules');
    res.json(result.rows);
  } catch (err) {
    res.status(500).json({ error: 'Error fetching rules' });
  }
};

exports.createRule = async (req, res) => {
  const { name, content } = req.body;
  try {
    const result = await db.query(
      'INSERT INTO rules (name, content) VALUES ($1, $2) RETURNING *',
      [name, content]
    );
    await prologService.updateRules();
    res.status(201).json(result.rows[0]);
  } catch (err) {
    res.status(500).json({ error: 'Error creating rule' });
  }
};

exports.updateRule = async (req, res) => {
  const { id } = req.params;
  const { name, content } = req.body;
  try {
    const result = await db.query(
      'UPDATE rules SET name = $1, content = $2 WHERE id = $3 RETURNING *',
      [name, content, id]
    );
    await prologService.updateRules();
    res.json(result.rows[0]);
  } catch (err) {
    res.status(500).json({ error: 'Error updating rule' });
  }
};

exports.deleteRule = async (req, res) => {
  const { id } = req.params;
  try {
    await db.query('DELETE FROM rules WHERE id = $1', [id]);
    await prologService.updateRules();
    res.status(204).send();
  } catch (err) {
    res.status(500).json({ error: 'Error deleting rule' });
  }
};

exports.applyRules = async (req, res) => {
  try {
    const result = await prologService.applyRules();
    res.json(result);
  } catch (err) {
    res.status(500).json({ error: 'Error applying rules' });
  }
};
