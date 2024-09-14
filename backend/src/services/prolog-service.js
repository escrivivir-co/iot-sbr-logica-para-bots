const swipl = require('swipl');
const logger = require('../utils/logger');
const templateService = require('./template-service')
const path = require('path');
const fs = require('fs');

class PrologService {

	last = [];

  	constructor() {
    	this.initialized = false;
  	}

  	async init(templateName) {
    	try {

			/**
			 *
			 */
			for(let l of this.last) {
				if (!l) continue;
				const unloadQuery = `unload_file('${l}')`;
				console.log("unloadQuery<<<<<<<<<<<<<<", unloadQuery)
				const d = await swipl.call(unloadQuery);
			}
			this.last = [];

			// Initialize the Prolog engine
			const templateF = path.join(templateService.APP_PATH, templateName + '.template');
			console.log("Read file", templateF)
			const data = await fs.readFileSync(templateF);
			console.log(data.toString())
			const template = JSON.parse(data.toString());

			console.log(template);
			for(const c of (template?.files || [])) {

				const moduleF = path.join(templateService.APP_PATH, templateName, c);
				const pls = await fs.readdirSync(moduleF).filter(p => p.indexOf('.pl') > -1);
				pls.forEach(async p => {
					const file = path.join(moduleF, p);
					const consultQuery = `consult('${file}')`;
					const e = await swipl.call(consultQuery);
					this.last.push(file)
					console.log("Consult>>>>>>>>>", file)
				})

			}

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
			return this.parseResult(result.Result);
		} catch (error) {
			console.error("Error executing Prolog query:", error.message);

			if ((error.message + '').includes('Unknown procedure')) {
			throw new Error("The procedure was not found!")
			}
			throw error;
		}
	}

	parseResult(data) {

		try {
			const myData = JSON.parse(data);
			// console.log("The data >>", myData, "ZZ")
			if (Array.isArray(myData)) {
				// console.log("The data is array", myData.length)
				return myData.map(d => d)
			} if(typeof myData == "string") {
				// console.log("The data is NOT array", typeof myData)
				return [ { result: myData }];
			}
			return myData;
			const o = JSON.parse(data)
		} catch(Ex) {
			console.log("Parse ", Ex.message)
			return [ { result: data }];
		}
		return [data];
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
