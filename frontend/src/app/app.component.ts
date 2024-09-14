import { Component, ViewChild } from '@angular/core';
import { RuleListComponent } from './components/rule-list/rule-list.component';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'Prolog Rule Management';

  @ViewChild(RuleListComponent) ruleListComponent!: RuleListComponent;

  onRuleSaved() {
    // Trigger a refresh of the rule list
    this.refreshRuleList();
  }

  refreshRuleList() {
    // Get a reference to the RuleListComponent
    const ruleListComponent = this.ruleListComponent;
    if (ruleListComponent) {
      ruleListComponent.loadRules();
    }
  }
}
