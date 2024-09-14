import { Component, Output, EventEmitter, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

/*
  {
            "predicate": "do_init",
            "arity": 2,
            "example": "do_init(Arg1, Arg2)",
            "evalCompatible": "eval(\"do_init(Arg1, Arg2)\")",
			"app": "app-id"
}
*/
@Component({
  selector: 'app-rule-editor',
  templateUrl: './rule-editor.component.html',
  styleUrls: ['./rule-editor.component.css']
})
export class RuleEditorComponent implements OnInit {
  	ruleText: string = '';
  	result: string = '';
  	sdkTemplates: any[] = [];
  	selectedTemplate: any = {};
  	@Output() ruleSaved = new EventEmitter<any>();
	@Output() appSelected = new EventEmitter<string>();

  	rules: any[] = [];
	rule = {};
  constructor(private prologService: PrologService) {}

  ngOnInit() {
    this.loadSdkTemplates();
  }

  loadSdkTemplates() {
    this.prologService.getSdkTemplates().subscribe(
      (templates) => {
		console.log("loading templtes", templates[0]?.name)
        this.sdkTemplates = templates;
      },
      (error) => {
        console.error('Error loading SDK templates:', error);
      }
    );
  }

  onTemplateSelect() {

    if (this.selectedTemplate) {
		console.log("", this.selectedTemplate)
		this.prologService.getTemplateContent(this.selectedTemplate).subscribe(
		(response) => {
			this.rules = response.content as unknown as any[];
			this.appSelected.emit(this.selectedTemplate)
		},
		(error) => {
			console.error('Error loading template content:', error);
		}
		);
    }
  }

	saveRule() {

		console.log("save rule", this.rule)
		this.prologService.saveRule(this.rule).subscribe(
			(response) => {
			this.result = 'Rule saved successfully';
			this.ruleText = '';
			this.ruleSaved.emit(this.rule);
			},
			(error) => {
			this.result = 'Error saving rule: ' + error.message;
			}
		);

  	}

  runRule() {
    this.prologService.runRule(this.ruleText).subscribe(
      (response) => {
        this.result = 'Rule execution result: ' + JSON.stringify(response);
      },
      (error) => {
        this.result = 'Error running rule: ' + error.message;
      }
    );
  }

  kitRun(rule: any, event: any) {

  }

  kitDelete(rule: any, event: any){}

  onSubmit(rule: any, event: any) {

    const formData = new FormData(event.target);
    let args: string[] = [];

    // Recopilar los argumentos de los inputs
    for (let i = 0; i < rule.arity; i++) {
      const argdata = formData.get(`arg${i + 1}`);
      args.push(argdata + '');
    }

    // Mapear los argumentos al campo `example`
    let exampleCall = rule.example;
    args.forEach((arg, index) => {
      exampleCall = exampleCall.replace(`Arg${index + 1}`, arg);
    });

    console.log("Llamada generada:", exampleCall);

    // Aquí puedes ejecutar el código generado o enviarlo a otro servicio
	rule.evalCompatible = exampleCall;
	return exampleCall;
  }

  kitSave(rule: any, event: any) {
	this.rule = rule;
	this.saveRule();
  }
}
