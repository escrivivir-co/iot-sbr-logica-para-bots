const fs = require('fs').promises;
const path = require('path');
const logger = require('../utils/logger');

const SDK_PATH = path.join(__dirname, '../../prolog/codigo/sdk');
const APP_PATH = path.join(__dirname, '../../prolog/codigo/app');

async function getSdkTemplates() {
  try {
    const files = await fs.readdir(SDK_PATH);
    return files.filter(file => file.endsWith('.pl'));
  } catch (error) {
    logger.error('Error reading SDK templates:', error);
    return [];
  }
}

async function getTemplateContent(templateName) {
  try {
    const content = await fs.readFile(path.join(SDK_PATH, templateName), 'utf8');
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
