import { Component, OnInit } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

interface SensorPreset {
  name: string;
  type: string;
  unit: string;
  defaultValue: number;
  min: number;
  max: number;
}

/**
 * Telemetry Process Component
 * 
 * Send telemetry data for IoT testing.
 * Endpoint covered: POST /telemetry/process
 * 
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-telemetry-process',
  templateUrl: './telemetry-process.component.html',
  styleUrls: ['./telemetry-process.component.css']
})
export class TelemetryProcessComponent implements OnInit {
  // Form
  sensorName: string = '';
  sensorValue: string = '';
  
  // State
  isProcessing: boolean = false;
  result: any = null;
  error: string | null = null;
  
  // History
  history: Array<{ sensor: string; value: string; timestamp: Date; success: boolean }> = [];
  
  // Presets
  readonly SENSOR_PRESETS: SensorPreset[] = [
    { name: 'temperature', type: 'number', unit: '°C', defaultValue: 22, min: -40, max: 60 },
    { name: 'humidity', type: 'number', unit: '%', defaultValue: 50, min: 0, max: 100 },
    { name: 'pressure', type: 'number', unit: 'hPa', defaultValue: 1013, min: 900, max: 1100 },
    { name: 'light', type: 'number', unit: 'lux', defaultValue: 500, min: 0, max: 10000 },
    { name: 'motion', type: 'boolean', unit: '', defaultValue: 0, min: 0, max: 1 },
    { name: 'door', type: 'state', unit: '', defaultValue: 0, min: 0, max: 1 },
    { name: 'co2', type: 'number', unit: 'ppm', defaultValue: 400, min: 0, max: 5000 },
    { name: 'noise', type: 'number', unit: 'dB', defaultValue: 40, min: 0, max: 120 }
  ];

  constructor(private prologService: PrologService) {}

  ngOnInit(): void {}

  /**
   * Process telemetry data
   */
  process(): void {
    if (!this.sensorName.trim()) {
      this.error = 'Sensor name is required';
      return;
    }
    
    if (!this.sensorValue.trim()) {
      this.error = 'Sensor value is required';
      return;
    }
    
    this.isProcessing = true;
    this.error = null;
    this.result = null;
    
    // Parse value (number or string)
    let value: number | string = this.sensorValue;
    const numValue = parseFloat(this.sensorValue);
    if (!isNaN(numValue)) {
      value = numValue;
    }
    
    this.prologService.processTelemetry({ 
      sensor: this.sensorName, 
      value 
    }).subscribe({
      next: (response) => {
        this.result = response;
        this.addToHistory(true);
        this.isProcessing = false;
      },
      error: (err) => {
        this.error = 'Error processing telemetry: ' + (err.error?.error || err.message || 'Unknown error');
        this.addToHistory(false);
        this.isProcessing = false;
      }
    });
  }

  /**
   * Apply sensor preset
   */
  applyPreset(preset: SensorPreset): void {
    this.sensorName = preset.name;
    this.sensorValue = preset.defaultValue.toString();
  }

  /**
   * Generate random value for current sensor
   */
  randomizeValue(): void {
    const preset = this.SENSOR_PRESETS.find(p => p.name === this.sensorName);
    if (preset) {
      const range = preset.max - preset.min;
      const random = Math.random() * range + preset.min;
      this.sensorValue = preset.type === 'number' 
        ? random.toFixed(1) 
        : (Math.random() > 0.5 ? '1' : '0');
    } else {
      this.sensorValue = (Math.random() * 100).toFixed(2);
    }
  }

  /**
   * Add entry to history
   */
  private addToHistory(success: boolean): void {
    this.history.unshift({
      sensor: this.sensorName,
      value: this.sensorValue,
      timestamp: new Date(),
      success
    });
    
    // Keep only last 10 entries
    if (this.history.length > 10) {
      this.history = this.history.slice(0, 10);
    }
  }

  /**
   * Clear history
   */
  clearHistory(): void {
    this.history = [];
  }

  /**
   * Get unit for current sensor
   */
  getCurrentUnit(): string {
    const preset = this.SENSOR_PRESETS.find(p => p.name === this.sensorName);
    return preset?.unit || '';
  }
}
