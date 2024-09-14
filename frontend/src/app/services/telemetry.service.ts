import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class TelemetryService {
	private apiUrl = 'https://' + window.location.hostname + ':8000' + '/iot';

  constructor(private http: HttpClient) {}

  getTelemetryStatus(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/status`);
  }

  processTelemetry(telemetry: any): Observable<any> {
    return this.http.post<any>(`${this.apiUrl}/process`, { telemetry });
  }
}
