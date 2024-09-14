import { Component, Input, OnInit, SimpleChanges } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

@Component({
  selector: 'app-rule-list',
  templateUrl: './rule-list.component.html',
  styleUrls: ['./rule-list.component.css']
})
export class RuleListComponent implements OnInit {

	@Input() theApp: any = { app: '' }; // Input value from the parent component
  	rules: any[] = [];
	result: any;
	@Input() data: any[] = []; // Entrada de array de objetos

  	constructor(private prologService: PrologService) {

	}

	ngOnInit() {

	}

	ngOnChanges(changes: SimpleChanges) {
		// Check if inputValue has changed
		if (changes['theApp'] && !changes['theApp'].isFirstChange()) {
			console.log("Refresh for AAddddd", this.theApp)	
			this.loadRules(this.theApp.app); // Trigger load function when inputValue changes
		}

	}

	escapeHtml(str: string) {

		if (typeof str !== 'string') {
		  return str; // If not a string, return as is
		}

		return str.replace(/[\u00A0-\u9999<>&]/gim, function(i) {
		  return `&#${i.charCodeAt(0)};`;
		});
	}

	objectKeysFalt(item: any) {

		if (!item) return [];
		if (item.length > 0) {

			const pattern = item[0];

			return Object.keys(pattern);

		} else {
			return []
		}

	}

	objectKeys() {
		if (this.data.length > 0) {

			const pattern = this.data[0];

			return Object.keys(pattern);

		} else {
			return []
		}

	}

	isObject(item: any) {
		return typeof item != "string"
	}

	loadRules(app: string) {

		if (!app) {
			this.rules = [];
			return;
		}
		console.log("Refresh for AA", this.theApp)
		this.prologService.getRules(app).subscribe(
		(rules) => {
			this.rules = rules;
		},
		(error) => {
			console.error('Error loading rules:', error);
		}
		);
	}

	deleteRule(id: number) {
		console.log("Refresh for 1", this.theApp)
		this.prologService.deleteRule(id).subscribe(
		() => {
			console.log("Refresh for", this.theApp)
			this.loadRules(this.theApp.app);
		},
		(error) => {
			console.error('Error deleting rule:', error);
		}
		);
  	}

	runRule(id: number) {

		const r = this.rules.find(r => r.id == id)
		if (!r) {
			console.log("No rule with id", id)
			return;
		}
		this.prologService.runRule(r.evalCompatible).subscribe(
			(result) => {
				this.result = result;
				this.data = this.result?.payload;
				console.log("After DATA IS FILLED", this.data)
			},
			(error) => {
				console.error('Error running rule:', error.message);
			}
		);

	}

	getResultText() {
		return JSON.stringify(this.result?.payload)
	}
}
