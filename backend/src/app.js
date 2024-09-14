const express = require('express');
const cors = require('cors');
const sqlite3 = require('sqlite3').verbose();
const mqtt = require('mqtt');
const swipl = require('swipl');
const logger = require('./utils/logger');
const config = require('../config');
const apiRoutes = require('./routes/api');
const telemetryRoutes = require('./routes/telemetry');
const mqttService = require('./services/mqtt-service');

const app = express();
const port = process.env.PORT || 8000;

app.use(cors({
  //origin: ["*"],
  //origin: [/\.repl\.co$/, /\.replit\.com$/],
  //credentials: true,
  //optionsSuccessStatus: 200
}));

app.use(express.json());

const db = new sqlite3.Database('./database.sqlite', (err) => {
  if (err) {
    logger.error('Error connecting to SQLite database:', err);
  } else {
    logger.info('Connected to SQLite database');
    // Create the rules table if it doesn't exist
    db.run(`CREATE TABLE IF NOT EXISTS rulesControl (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
	  predicate TEXT NOT NULL,
	  arity TEXT NOT NULL,
	  example TEXT NOT NULL,
	  evalCompatible TEXT NOT NULL,
	  app TEXT NOT NULL
    )`);

    // Add the simple test query
    db.all('SELECT * FROM rules LIMIT 1', (err, rows) => {
      if (err) {
        logger.error('Error querying database:', err);
      } else {
        logger.info('Database query successful. Number of rules:', rows.length);
      }
    });
  }
});

// MQTT connection
const mqttClient = mqtt.connect(config.mqttBroker, {
  username: config.mqttUsername,
  password: config.mqttPassword
});

mqttClient.on('connect', () => {
  logger.info('Connected to MQTT broker');
  mqttService.init(mqttClient);
});

mqttClient.on('error', (error) => {
  logger.error('MQTT connection error:', error);
});

// Routes
app.use('/api', apiRoutes);
app.use('/api/telemetry', telemetryRoutes);

// Error handling middleware
app.use((err, req, res, next) => {
  logger.error(err.stack);
  res.status(500).send('Something went wrong!');
});

// Start the server
app.listen(port, '0.0.0.0', () => {
  logger.info(`Server is now running and listening on http://0.0.0.0:${port}`);
});

module.exports = app;
