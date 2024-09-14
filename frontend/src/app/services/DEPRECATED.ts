import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Rule } from '../models/rule.model';
import { environment } from '../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class RuleService {
  private apiUrl = `${environment.apiUrl}/rules`;

  constructor(private http: HttpClient) {
	console.log(this.apiUrl)
  }

  getRules(): Observable<Rule[]> {
    return this.http.get<Rule[]>(this.apiUrl);
  }

  createRule(rule: Rule): Observable<Rule> {
    return this.http.post<Rule>(this.apiUrl, rule);
  }

  updateRule(rule: Rule): Observable<Rule> {
    return this.http.put<Rule>(`${this.apiUrl}/${rule.id}`, rule);
  }

  deleteRule(id: number): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/${id}`);
  }

  applyRules(): Observable<any> {
    return this.http.post<any>(`${this.apiUrl}/apply`, {});
  }
}
