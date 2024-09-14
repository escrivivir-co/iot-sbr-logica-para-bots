import { Component, Output, EventEmitter, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

/*
  {
            "predicate": "do_init",
            "arity": 2,
            "example": "do_init(Arg1, Arg2)",
            "evalCompatible": "eval(\"do_init(Arg1, Arg2)\")"
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
  sdkTemplates: string[] = [];
  selectedTemplate: string = '';
  @Output() ruleSaved = new EventEmitter<void>();

  rules: any[] = [];

  constructor(private prologService: PrologService) {}

  ngOnInit() {
    this.loadSdkTemplates();
  }

  loadSdkTemplates() {
    this.prologService.getSdkTemplates().subscribe(
      (templates) => {
        this.sdkTemplates = templates;
      },
      (error) => {
        console.error('Error loading SDK templates:', error);
      }
    );
  }

  onTemplateSelect() {
    if (this.selectedTemplate) {
      this.prologService.getTemplateContent(this.selectedTemplate).subscribe(
        (response) => {
          this.rules = response.content as unknown as any[];
        },
        (error) => {
          console.error('Error loading template content:', error);
        }
      );
    }
  }

  saveRule() {
    this.prologService.saveRule(this.ruleText).subscribe(
      (response) => {
        this.result = 'Rule saved successfully';
        this.ruleText = '';
        this.ruleSaved.emit();
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
	this.ruleText = rule.evalCompatible;
	this.saveRule();
  }
}
