import { Component, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

@Component({
  selector: 'app-rule-list',
  templateUrl: './rule-list.component.html',
  styleUrls: ['./rule-list.component.css']
})
export class RuleListComponent implements OnInit {
  rules: any[] = [];

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
}
