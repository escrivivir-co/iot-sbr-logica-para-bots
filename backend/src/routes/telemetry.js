const express = require('express');
const telemetryController = require('../controllers/telemetry-controller');

const router = express.Router();

router.post('/process', telemetryController.processTelemetry);
router.get('/status', telemetryController.getTelemetryStatus);

module.exports = router;
