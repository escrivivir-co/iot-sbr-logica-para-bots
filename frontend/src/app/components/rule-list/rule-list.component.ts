import { Component, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

@Component({
  selector: 'app-rule-list',
  templateUrl: './rule-list.component.html',
  styleUrls: ['./rule-list.component.css']
})
export class RuleListComponent implements OnInit {
  rules: any[] = [];
	result: any;

  constructor(private prologService: PrologService) {}

  ngOnInit() {
    this.loadRules();
  }

  loadRules() {
    this.prologService.getRules().subscribe(
      (rules) => {
        this.rules = rules;
      },
      (error) => {
        console.error('Error loading rules:', error);
      }
    );
  }

  deleteRule(id: number) {
    this.prologService.deleteRule(id).subscribe(
      () => {
        this.loadRules();
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
    this.prologService.runRule(r.text).subscribe(
      (result) => {
		this.result = result;
        console.log("After", result)
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
