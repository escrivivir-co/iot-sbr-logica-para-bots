import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { FormsModule } from '@angular/forms';

import { AppComponent } from './app.component';
import { RuleEditorComponent } from './components/rule-editor/rule-editor.component';
import { RuleListComponent } from './components/rule-list/rule-list.component';
import { PrologService } from './services/prolog.service';
import { DashboardComponent } from './components/dashboard/dashboard.component';
import { TelemetryMonitorComponent } from './components/telemetry-monitor/telemetry-monitor.component';

// New MCP-aligned components (PROLOG-UI-2.0.0)
import { SessionManagerComponent } from './components/session-manager/session-manager.component';
import { KnowledgeBaseComponent } from './components/knowledge-base/knowledge-base.component';
import { McpTemplatesBrowserComponent } from './components/mcp-templates-browser/mcp-templates-browser.component';
import { UserAppSaveDialogComponent } from './components/user-app-save-dialog/user-app-save-dialog.component';
import { TelemetryProcessComponent } from './components/telemetry-process/telemetry-process.component';

// Teatro integration (TEATRO-PROLOG-1.0.0)
import { BrainEditorComponent } from './components/brain-editor/brain-editor.component';

@NgModule({
  declarations: [
    AppComponent,
    RuleEditorComponent,
    RuleListComponent,
    TelemetryMonitorComponent,
    DashboardComponent,
    // New components
    SessionManagerComponent,
    KnowledgeBaseComponent,
    McpTemplatesBrowserComponent,
    UserAppSaveDialogComponent,
    TelemetryProcessComponent,
    // Teatro integration
    BrainEditorComponent
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule
  ],
  providers: [PrologService],
  bootstrap: [AppComponent]
})
export class AppModule { }
