import { Component, Output, EventEmitter, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

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
          this.ruleText = response.content;
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
}
