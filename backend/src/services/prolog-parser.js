const fs = require('fs');
const path = require('path');

const APP_PATH = path.join(__dirname, 'codigo/sdk/modulos/web');

class PrologParser {

	constructor(filePath) {

        this.filePath = path.join(APP_PATH, filePath);
        this.prologCode = '';
        this.exports = [];
    }

	telemetryToPrologFacts = (telemetry) => {
		const facts = [];
		for (const [key, value] of Object.entries(telemetry)) {
		  facts.push(`telemetry(${key}, ${value})`);
		}
		return facts;
	  };

    // Leer el fichero .pl y cargar el contenido
    async loadPrologFile() {
        return new Promise((resolve, reject) => {
            fs.readFile(this.filePath, 'utf8', (err, data) => {
				console.log(this.filePath, data)
                if (err) {
                    return reject(err);
                }
                this.prologCode = data;
                resolve(this.prologCode);
            });
        });
    }

    // Extraer los predicados exportados del módulo Prolog
    extractExports() {
        const exportRegex = /:-\s*module\([^,]+,\s*\[([^\]]+)\]\)./;
        const match = this.prologCode.match(exportRegex);
        if (match && match[1]) {
            const exports = match[1].split(',').map(exp => exp.trim());
            this.exports = exports;
        }
    }

    // Generar una lista de objetos JSON con llamadas de ejemplo para eval
    generateJsonExamples() {
        return this.exports.map((exp) => {
			console.log("El export", exp)
            const [name, arity] = exp.split('/');
            const args = Array.from({ length: parseInt(arity, 10) }, (_, i) => `Arg${i + 1}`);
            const exampleCall = `${name}(${args.join(', ')})`;
            return {
                predicate: name,
                arity: parseInt(arity, 10),
                example: exampleCall,
                evalCompatible: `eval("${exampleCall}")`
            };
        });
    }

    // Ejecutar el proceso completo: leer fichero, extraer exports, generar JSON
    async parseFile() {
        await this.loadPrologFile();
        this.extractExports();
        const examples = this.generateJsonExamples();
        return examples;
    }
}

module.exports = {
	PrologParser
  };