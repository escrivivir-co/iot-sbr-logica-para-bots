const fs = require('fs').promises;
const path = require('path');
const logger = require('../utils/logger');
const prologParser = require('../services/prolog-parser');

const SDK_PATH = path.join(__dirname, 'codigo/sdk');
const APP_PATH = path.join(__dirname, 'codigo/sdk/modulos/web');

async function getSdkTemplates() {
  try {
    const files = await fs.readdir(APP_PATH);
    return files.filter(file => file.endsWith('.pl'));
  } catch (error) {
    logger.error('Error reading SDK templates:', error);
    return [];
  }
}

async function getTemplateContent(templateName) {
  try {
    // const content = await fs.readFile(path.join(APP_PATH, templateName), 'utf8');
	console.log("Getting the parsed pl")
	const p = new prologParser.PrologParser(templateName);
	const content = p.parseFile();
    return content;
  } catch (error) {
    logger.error(`Error reading template ${templateName}:`, error);
    return null;
  }
}

async function saveUserApp(appName, content) {
  try {
    await fs.writeFile(path.join(APP_PATH, `${appName}.pl`), content);
    return true;
  } catch (error) {
    logger.error(`Error saving user app ${appName}:`, error);
    return false;
  }
}

module.exports = {
  getSdkTemplates,
  getTemplateContent,
  saveUserApp
};
