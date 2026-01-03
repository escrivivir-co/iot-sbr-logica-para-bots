import { Component, ViewChild } from '@angular/core';
import { RuleListComponent } from './components/rule-list/rule-list.component';
import { PrologService } from './services/prolog.service';

/**
 * Main Application Component
 * 
 * Provides tabbed navigation for all MCP-aligned features.
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'PrologEditor';
  theApp: any = { app: '' };
  
  // Navigation
  activeTab: 'sessions' | 'editor' | 'knowledge' | 'templates' | 'telemetry' = 'sessions';
  
  // Session state
  currentSessionId: string | null = null;
  
  // Save dialog state
  showSaveDialog: boolean = false;
  saveDialogContent: string = '';

  @ViewChild(RuleListComponent) ruleListComponent!: RuleListComponent;

  constructor(private prologService: PrologService) {
    // Initialize session from service
    this.currentSessionId = this.prologService.getCurrentSession();
  }

  onRuleSaved(rule: any) {
    this.refreshRuleList(rule?.app);
  }

  appSelected(app: string) {
    this.refreshRuleList(app);
  }

  refreshRuleList(app: string) {
    this.theApp = { app };
  }

  // Session management
  onSessionSelected(sessionId: string) {
    this.currentSessionId = sessionId;
  }

  onSessionCreated(session: any) {
    this.currentSessionId = session.sessionId;
    // Switch to editor tab after creating session
    this.activeTab = 'editor';
  }

  // Template handling
  onTemplateLoaded(template: any) {
    // Switch to editor after loading template
    this.activeTab = 'editor';
  }

  // Save dialog
  openSaveDialog(content: string) {
    this.saveDialogContent = content;
    this.showSaveDialog = true;
  }

  onAppSaved(event: { name: string; content: string }) {
    this.showSaveDialog = false;
    // Refresh rule list
    this.refreshRuleList(event.name);
  }
}
