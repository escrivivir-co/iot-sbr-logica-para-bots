import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class PrologService {
  private apiUrl = environment.apiUrl;

  constructor(private http: HttpClient) {}

  saveRule(ruleText: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/rules`, { text: ruleText });
  }

  runRule(ruleText: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/run-rule`, { text: ruleText });
  }

  getRules(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/rules`);
  }

  deleteRule(id: number): Observable<any> {
    return this.http.delete(`${this.apiUrl}/rules/${id}`);
  }

  getSdkTemplates(): Observable<string[]> {
    return this.http.get<string[]>(`${this.apiUrl}/sdk-templates`);
  }

  getTemplateContent(templateName: string): Observable<{content: string}> {
    return this.http.get<{content: string}>(`${this.apiUrl}/template/${templateName}`);
  }
}
