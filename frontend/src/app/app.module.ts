import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { FormsModule } from '@angular/forms';

import { AppComponent } from './app.component';
import { DashboardComponent } from './components/dashboard/dashboard.component';
import { RuleEditorComponent } from './components/rule-editor/rule-editor.component';
import { TelemetryMonitorComponent } from './components/telemetry-monitor/telemetry-monitor.component';

@NgModule({
  declarations: [
    AppComponent,
    DashboardComponent,
    RuleEditorComponent,
    TelemetryMonitorComponent
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
