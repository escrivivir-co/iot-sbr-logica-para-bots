import { Component, OnInit } from '@angular/core';
import { RuleService } from '../../services/rule.service';
import { Rule } from '../../models/rule.model';

@Component({
  selector: 'app-rule-editor',
  templateUrl: './rule-editor.component.html',
  styleUrls: ['./rule-editor.component.css']
})
export class RuleEditorComponent implements OnInit {
  rules: Rule[] = [];
  newRule: Rule = { name: '', content: '' };

  constructor(private ruleService: RuleService) {}

  ngOnInit() {
    this.loadRules();
  }

  loadRules() {
    this.ruleService.getRules().subscribe(
      (rules) => this.rules = rules,
      (error) => console.error('Error loading rules:', error)
    );
  }

  createRule() {
    this.ruleService.createRule(this.newRule).subscribe(
      (rule) => {
        this.rules.push(rule);
        this.newRule = { name: '', content: '' };
      },
      (error) => console.error('Error creating rule:', error)
    );
  }

  updateRule(rule: Rule) {
    this.ruleService.updateRule(rule).subscribe(
      (updatedRule) => {
        const index = this.rules.findIndex(r => r.id === updatedRule.id);
        if (index !== -1) {
          this.rules[index] = updatedRule;
        }
      },
      (error) => console.error('Error updating rule:', error)
    );
  }

  deleteRule(rule: Rule) {
    this.ruleService.deleteRule(rule.id).subscribe(
      () => {
        this.rules = this.rules.filter(r => r.id !== rule.id);
      },
      (error) => console.error('Error deleting rule:', error)
    );
  }

  applyRules() {
    this.ruleService.applyRules().subscribe(
      (result) => console.log('Rules applied:', result),
      (error) => console.error('Error applying rules:', error)
    );
  }
}
