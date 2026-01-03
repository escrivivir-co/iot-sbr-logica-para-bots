import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../environments/environment';
import type { 
  Rule, 
  RuleInput, 
  RuleCreatedResponse 
} from '../models/rule.model';
import type { 
  QueryResponse 
} from '../models/query.model';
import type { 
  PrologSession, 
  CreateSessionRequest, 
  SessionResponse, 
  ListSessionsResponse 
} from '../models/session.model';

/**
 * Service for Prolog operations.
 * 
 * Connects to the REST API backend which delegates to MCP Prolog Server.
 */
@Injectable({
  providedIn: 'root'
})
export class PrologService {
  private apiUrl = environment.apiUrl;
  templateName: string = "";
  currentSessionId: string | null = null;

  constructor(private http: HttpClient) {}

  // ============================================
  // Rule CRUD Operations
  // ============================================

  saveRule(rule: RuleInput): Observable<RuleCreatedResponse> {
    return this.http.post<RuleCreatedResponse>(`${this.apiUrl}/rules`, rule);
  }

  getRules(app: string): Observable<Rule[]> {
    return this.http.get<Rule[]>(`${this.apiUrl}/rules/${app}`);
  }

  deleteRule(id: number): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/rules/${id}`);
  }

  // ============================================
  // Query Execution
  // ============================================

  runRule(ruleText: string, sessionId?: string): Observable<QueryResponse> {
    return this.http.post<QueryResponse>(`${this.apiUrl}/run-rule`, { 
      text: ruleText,
      sessionId: sessionId || this.currentSessionId
    });
  }

  // ============================================
  // Template Operations
  // ============================================

  getSdkTemplates(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/sdk-templates`);
  }

  getTemplateContent(templateName: string): Observable<{content: string}> {
    this.templateName = templateName;
    return this.http.get<{content: string}>(`${this.apiUrl}/template/${templateName}`);
  }

  getMcpTemplates(): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/mcp-templates`);
  }

  // ============================================
  // Session Management (MCP)
  // ============================================

  createSession(sessionId: string, obraId: string): Observable<SessionResponse> {
    return this.http.post<SessionResponse>(`${this.apiUrl}/sessions`, {
      sessionId,
      obraId
    });
  }

  listSessions(): Observable<ListSessionsResponse> {
    return this.http.get<ListSessionsResponse>(`${this.apiUrl}/sessions`);
  }

  destroySession(sessionId: string): Observable<SessionResponse> {
    return this.http.delete<SessionResponse>(`${this.apiUrl}/sessions/${sessionId}`);
  }

  // ============================================
  // Prolog Operations (MCP)
  // ============================================

  assertFact(fact: string, sessionId?: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/assert`, {
      sessionId: sessionId || this.currentSessionId,
      fact
    });
  }

  consultFile(filePath: string, sessionId?: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/consult`, {
      sessionId: sessionId || this.currentSessionId,
      filePath
    });
  }

  // ============================================
  // Session State
  // ============================================

  setCurrentSession(sessionId: string): void {
    this.currentSessionId = sessionId;
  }

  getCurrentSession(): string | null {
    return this.currentSessionId;
  }
}
