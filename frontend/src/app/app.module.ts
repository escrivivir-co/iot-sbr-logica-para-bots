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

@NgModule({
  declarations: [
    AppComponent,
    RuleEditorComponent,
    RuleListComponent,
    TelemetryMonitorComponent,
	  DashboardComponent
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
