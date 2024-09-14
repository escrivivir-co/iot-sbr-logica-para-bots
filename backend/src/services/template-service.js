const fs = require('fs').promises;
const path = require('path');
const logger = require('../utils/logger');
const prologParser = require('../services/prolog-parser');

const APP_PATH = path.join(__dirname, 'codigo/web/plugins');

async function getSdkTemplates() {
  try {
    const files = await fs.readdir(APP_PATH);
    const templates = files.filter(file => file.endsWith('.template'));

	const tmps = []
	for(let t of templates) {
		const data = await fs.readFile(path.join(APP_PATH, t));
		const o = JSON.parse(data.toString());
		console.log("Readed", path.join(APP_PATH, t), "with", o)
		tmps.push(o)
	}
	return tmps;

  } catch (error) {
    logger.error('Error reading SDK templates:', error);
    return [];
  }
}

async function getTemplateContent(templateName) {
  try {
    // const content = await fs.readFile(path.join(APP_PATH, templateName), 'utf8');
	const p = new prologParser.PrologParser(templateName);
	console.log("Getting the parsed pl", p.filePath)
	const content = p.parseFile(templateName);
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
  saveUserApp,
  APP_PATH
};
