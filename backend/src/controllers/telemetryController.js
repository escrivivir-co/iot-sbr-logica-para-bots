const prologService = require('../services/prologService');
const prologParser = require('../utils/prologParser');

exports.processTelemetry = async (req, res) => {
  const { telemetry } = req.body;
  try {
    const prologFacts = prologParser.telemetryToPrologFacts(telemetry);
    await prologService.assertFacts(prologFacts);
    const result = await prologService.applyRules();
    res.json(result);
  } catch (err) {
    res.status(500).json({ error: 'Error processing telemetry' });
  }
};

exports.getTelemetryStatus = async (req, res) => {
  try {
    const status = await prologService.getTelemetryStatus();
    res.json(status);
  } catch (err) {
    res.status(500).json({ error: 'Error getting telemetry status' });
  }
};
