const swipl = require("swipl");

class PrologService {
  constructor() {
    this.initialized = false;
  }

  async init() {
    try {
      // Initialize the Prolog engine
      this.initialized = true;
      console.log("Prolog engine initialized successfully");
    } catch (error) {
      console.error("Failed to initialize Prolog engine:", error);
      throw error;
    }
  }

  async query(goal) {
    if (!this.initialized) {
      throw new Error("Prolog engine not initialized");
    }

    try {
      // Use swipl.call to execute a query
      const result = await swipl.call(goal);
      return result;
    } catch (error) {
      console.error("Error executing Prolog query:", error);
      throw error;
    }
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
