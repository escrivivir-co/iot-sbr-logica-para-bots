import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class PrologService {
  private apiUrl = environment.apiUrl;
	templateName: string = "";

  constructor(private http: HttpClient) {
	console.log(this.apiUrl,  window.location.hostname)
  }

  saveRule(rule: any): Observable<any> {
    return this.http.post(`${this.apiUrl}/rules`, rule);
  }

  runRule(ruleText: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/run-rule`, { text: ruleText });
  }

  getRules(app: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/rules/${app}`);
  }

  deleteRule(id: number): Observable<any> {
    return this.http.delete(`${this.apiUrl}/rules/${id}`);
  }

  getSdkTemplates(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/sdk-templates`);
  }

  getTemplateContent(templateName: string): Observable<{content: string}> {
	console.log("Send", templateName)
	this.templateName = templateName;
    return this.http.get<{content: string}>(`${this.apiUrl}/template/${templateName}`);
  }
}
