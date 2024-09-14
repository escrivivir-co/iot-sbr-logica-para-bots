import { Component, ViewChild } from '@angular/core';
import { RuleListComponent } from './components/rule-list/rule-list.component';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  	title = 'Prolog Rule Management';
	theApp: any =  { app: '' };

	@ViewChild(RuleListComponent) ruleListComponent!: RuleListComponent;

	onRuleSaved(rule: any) {
		// Trigger a refresh of the rule list
		console.log("On saved", rule)
		this.refreshRuleList(rule?.app);
	}

	appSelected(app: string) {
		// Trigger a refresh of the rule list
		console.log("On appSelected", app)
		this.refreshRuleList(app);
	}

	refreshRuleList(app: string) {
		this.theApp = {
			app
		};
	}
}
