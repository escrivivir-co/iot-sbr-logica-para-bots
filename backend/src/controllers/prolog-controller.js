const prologService = require('../services/prolog-service');
const Rule = require('../models/rule');
const logger = require('../utils/logger');
const templateService = require('../services/template-service');

exports.saveRule = async (req, res) => {
  try {
    const rule = await Rule.create(req.body);
    res.status(201).json(rule);
  } catch (error) {
    logger.error('Error saving rule:', error);
    res.status(500).json({ error: 'Error saving rule' });
  }
};

exports.getRules = async (req, res) => {
  try {
	console.log("Prolog service", "Send query to server database...")
	const { id } = req.params;
    const rules = await Rule.getAll(id);
    res.json(rules);
  } catch (error) {
    console.log("PrologoController", error)
    logger.error('Error getting rules:', error);
    res.status(500).json({ error: 'Error getting rules' });
  }
};

exports.deleteRule = async (req, res) => {
  try {
    const { id } = req.params;
    await Rule.delete(id);
    res.status(204).send();
  } catch (error) {
    logger.error('Error deleting rule:', error);
    res.status(500).json({ error: 'Error deleting rule' });
  }
};

exports.runRule = async (req, res) => {
  try {
    const { text } = req.body;
    const result = await prologService.executeQuery(text);
	console.log("Send results", result)
    res.json({
		status: 200,
		payload: result
	});
  } catch (error) {
    console.log(error)
    res.status(500).json({ error: error.message });
  }
};

exports.getSdkTemplates = async (req, res) => {
  try {
    const templates = await templateService.getSdkTemplates();
	// console.log("Return templates", templates)
    res.json(templates);
  } catch (error) {
    logger.error('Error getting SDK templates:', error);
    res.status(500).json({ error: 'Error getting SDK templates' });
  }
};

exports.getTemplateContent = async (req, res) => {
  try {
    const { templateName } = req.params;
    const content = await templateService.getTemplateContent(templateName);

    if (content) {

	prologService.init(templateName);

      res.json({ content });
    } else {
      res.status(404).json({ error: 'Template not found' });
    }
  } catch (error) {
    logger.error('Error getting template content:', error);
    res.status(500).json({ error: 'Error getting template content' });
  }
};

exports.saveUserApp = async (req, res) => {
  try {
    const { appName, content } = req.body;
    const success = await templateService.saveUserApp(appName, content);
    if (success) {
      res.status(201).json({ message: 'App saved successfully' });
    } else {
      res.status(500).json({ error: 'Error saving app' });
    }
  } catch (error) {
    logger.error('Error saving user app:', error);
    res.status(500).json({ error: 'Error saving user app' });
  }
};
