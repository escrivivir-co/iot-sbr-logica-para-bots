const swipl = require('swipl');
const logger = require('../utils/logger');

class PrologService {
  constructor() {
    this.initialized = false;
  }

  async init(templateName) {
    try {
      // Initialize the Prolog engine

	  const path = __dirname + "/codigo/sdk/modulos/web/" + templateName;
	  const query = `consult('${path}')`;

	  
	  console.log("Prolog engine initialized process, try...",__dirname, query);
	  swipl.call(query);
	  this.initialized = true;
    } catch (error) {
      console.error("Failed to initialize Prolog engine:", error.message);
      throw error;
    }
  }

  async executeQuery(goal) {

    if (!this.initialized) {
      throw new Error("Prolog engine not initialized");
    }

    try {
      // Use swipl.call to execute a query
	  console.log("Searching query", goal);
      const result = await swipl.call(goal);
	  console.log("Object result raw[\n", result, "\n]");
	  return result;
    } catch (error) {
      console.error("Error executing Prolog query:", error.message);

	  if ((error.message + '').includes('Unknown procedure')) {
		throw new Error("The procedure was not found!")
	  }
      throw error;
    }
  }

  getTelemetryStatus() {
	return [{
		sensor: "light1",
		value: "on"
	}]
  }

  cleanup() {
    if (this.initialized) {
      swipl.cleanup();
      this.initialized = false;
      console.log("Prolog engine cleaned up");
    }
  }
}

module.exports = new PrologService();
