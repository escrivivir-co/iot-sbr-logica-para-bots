import { Component, OnInit } from '@angular/core';
import { TelemetryService } from '../../services/telemetry.service';

@Component({
  selector: 'app-telemetry-monitor',
  templateUrl: './telemetry-monitor.component.html',
  styleUrls: ['./telemetry-monitor.component.css']
})
export class TelemetryMonitorComponent implements OnInit {
  telemetryStatus: any[] = [];

  constructor(private telemetryService: TelemetryService) {}

  ngOnInit() {
    this.getTelemetryStatus();
  }

  getTelemetryStatus() {
    this.telemetryService.getTelemetryStatus().subscribe(
      (status) => this.telemetryStatus = status,
      (error) => console.error('Error getting telemetry status:', error)
    );
  }
}
