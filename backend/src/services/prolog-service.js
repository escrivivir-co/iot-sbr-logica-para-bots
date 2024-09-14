const swipl = require('swipl');
const logger = require('../utils/logger');

function executeRule(ruleText) {
  return new Promise((resolve, reject) => {
    try {
      // Asserting the rule
      swipl.call(`assert((${ruleText}))`);

      // Querying the rule
      const query = swipl.query('call((Goal))');
      const results = [];

      // Collect all results
      (function getNext() {
        query.next((success) => {
          if (success) {
            results.push(query.getBindings());
            getNext();
          } else {
            query.close();
            swipl.call(`retract((${ruleText}))`);
            resolve(results);
          }
        });
      })();
    } catch (error) {
      logger.error('Error executing Prolog rule:', error);
      reject(error);
    }
  });
}

module.exports = {
  executeRule
};
