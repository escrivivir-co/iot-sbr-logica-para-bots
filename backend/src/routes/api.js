const express = require('express');
const prologController = require('../controllers/prolog-controller');

const router = express.Router();

router.post('/rules', prologController.saveRule);
router.get('/rules', prologController.getRules);
router.delete('/rules/:id', prologController.deleteRule);
router.post('/run-rule', prologController.runRule);

router.get('/sdk-templates', prologController.getSdkTemplates);
router.get('/template/:templateName', prologController.getTemplateContent);
router.post('/user-app', prologController.saveUserApp);

module.exports = router;
