const express = require('express');
const ruleController = require('../controllers/ruleController');

const router = express.Router();

router.get('/', ruleController.getRules);
router.post('/', ruleController.createRule);
router.put('/:id', ruleController.updateRule);
router.delete('/:id', ruleController.deleteRule);
router.post('/apply', ruleController.applyRules);

module.exports = router;
