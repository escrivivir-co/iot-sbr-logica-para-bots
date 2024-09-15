const swipl = require('swipl');
const logger = require('../utils/logger');
const templateService = require('./template-service')
const path = require('path');
const fs = require('fs');
const { type } = require('os');

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

			if (typeof result == "object") {
				if (Object.keys(result) == 0) {
					return this.parseResult("Success");
				}
				return this.parseResult(result[Object.keys(result)[0]]);
			} else {
				return this.parseResult(result);
			}

		} catch (error) {
			console.error("Error executing Prolog query:", error.message);

			if ((error.message + '').includes('Unknown procedure')) {
			throw new Error("The procedure was not found!")
			}
			throw error;
		}
	}

	convertPairKeyToJSON(prologList) {
		const result = {};
		for (const pair of prologList) {
			result[pair[0]] = pair[1];  // 'pair' is an array with two elements: key and value
		}
		return result;
	}

	parseResult(data) {

		try {
			console.log("The data", typeof data, data)
			if (data === false || (!data)) {
				return [ { Result: "FALSE" }]
			}

			// console.log("An object found missing", typeof data)
			let myData = data;
			console.log("The data >>", myData, "ZZ")
			if (typeof myData == "string"){
				try {
					myData = JSON.parse(data);
					console.log("The object is a json string!!!!")
					return myData;
				} catch(ex) {
					console.log("The object is not  a json string")
				}
			}

			if (Array.isArray(myData)) {

				myData = JSON.parse(data);
				console.log("The data is array", myData.length)
				return myData.map(d => d)
			} else if(typeof myData == "string") {
				console.log("The data is NOT array", typeof myData)
				return [ { result: myData }];
			}

			if (typeof myData == "number") {
				return [ { result: myData + ""}];
			}
			console.log("The data is any", typeof myData)

			return [myData];

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
