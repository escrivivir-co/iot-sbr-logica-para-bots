const express = require('express');
const cors = require('cors');
const ruleRoutes = require('./routes/ruleRoutes');
const telemetryRoutes = require('./routes/telemetryRoutes');
const prologService = require('./services/prologService');
const mqttService = require('./services/mqttService');

const app = express();

app.use(cors({
  origin: '*',
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
  allowedHeaders: ['Content-Type', 'Authorization']
}));

app.use(express.json());

app.use('/api/rules', ruleRoutes);
app.use('/api/telemetry', telemetryRoutes);
// Enable trust proxy to work with Replit's proxy
app.set('trust proxy', 1);

// Middleware to handle Invalid Host header issue
app.use((req, res, next) => {
  const allowedHosts = ['0.0.0.0', 'localhost', '.repl.co'];
  const host = req.headers.host || '';
  if (allowedHosts.some(allowedHost => host.includes(allowedHost))) {
    next();
  } else {
    res.status(403).send('Forbidden');
  }
});

const PORT = process.env.PORT || 8000;

async function startServer() {
  try {
    await prologService.init();
    mqttService.connect();
    app.listen(PORT, '0.0.0.0', () => {
      console.log(`Server running on port ${PORT}`);
    });
  } catch (err) {
    console.error('Failed to start server:', err);
  }
}

startServer();
