import { Component, OnInit, OnDestroy, Output, EventEmitter } from '@angular/core';
import { PrologService } from '../../services/prolog.service';
import type { PrologSession } from '../../models/session.model';

/**
 * Session Manager Component
 * 
 * Exposes explicit control over MCP Prolog sessions.
 * Tools covered: prolog_create_session, prolog_list_sessions, prolog_destroy_session
 * 
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-session-manager',
  templateUrl: './session-manager.component.html',
  styleUrls: ['./session-manager.component.css']
})
export class SessionManagerComponent implements OnInit, OnDestroy {
  // Form fields
  newSessionId: string = '';
  newObraId: string = '';
  
  // State
  sessions: PrologSession[] = [];
  currentSessionId: string | null = null;
  isLoading: boolean = false;
  error: string | null = null;
  successMessage: string | null = null;
  
  // Auto-refresh
  private refreshInterval: any;
  readonly REFRESH_INTERVAL_MS = 30000; // 30 seconds
  
  @Output() sessionSelected = new EventEmitter<string>();
  @Output() sessionCreated = new EventEmitter<PrologSession>();
  @Output() sessionDestroyed = new EventEmitter<string>();

  constructor(private prologService: PrologService) {}

  ngOnInit(): void {
    this.loadSessions();
    this.currentSessionId = this.prologService.getCurrentSession();
    
    // Auto-refresh sessions list
    this.refreshInterval = setInterval(() => {
      this.loadSessions(true);
    }, this.REFRESH_INTERVAL_MS);
  }

  ngOnDestroy(): void {
    if (this.refreshInterval) {
      clearInterval(this.refreshInterval);
    }
  }

  /**
   * Load all active sessions
   */
  loadSessions(silent: boolean = false): void {
    if (!silent) {
      this.isLoading = true;
    }
    this.error = null;
    
    this.prologService.listSessions().subscribe({
      next: (response) => {
        this.sessions = response.sessions || [];
        this.isLoading = false;
      },
      error: (err) => {
        this.error = 'Error loading sessions: ' + (err.message || 'Unknown error');
        this.isLoading = false;
      }
    });
  }

  /**
   * Create a new Prolog session
   */
  createSession(): void {
    if (!this.newSessionId.trim()) {
      this.error = 'Session ID is required';
      return;
    }
    if (!this.newObraId.trim()) {
      this.error = 'Obra ID is required';
      return;
    }
    
    this.isLoading = true;
    this.error = null;
    this.successMessage = null;
    
    this.prologService.createSession(this.newSessionId, this.newObraId).subscribe({
      next: (response) => {
        this.successMessage = `Session "${this.newSessionId}" created successfully`;
        this.newSessionId = '';
        this.newObraId = '';
        this.loadSessions();
        
        // Auto-select the new session
        if (response.session) {
          this.selectSession(response.session.sessionId);
          this.sessionCreated.emit(response.session);
        }
      },
      error: (err) => {
        this.error = 'Error creating session: ' + (err.error?.error || err.message || 'Unknown error');
        this.isLoading = false;
      }
    });
  }

  /**
   * Destroy a session with confirmation
   */
  destroySession(sessionId: string): void {
    if (!confirm(`Are you sure you want to destroy session "${sessionId}"?`)) {
      return;
    }
    
    this.isLoading = true;
    this.error = null;
    this.successMessage = null;
    
    this.prologService.destroySession(sessionId).subscribe({
      next: () => {
        this.successMessage = `Session "${sessionId}" destroyed`;
        
        // Clear current session if destroyed
        if (this.currentSessionId === sessionId) {
          this.currentSessionId = null;
          this.prologService.setCurrentSession('');
        }
        
        this.loadSessions();
        this.sessionDestroyed.emit(sessionId);
      },
      error: (err) => {
        this.error = 'Error destroying session: ' + (err.error?.error || err.message || 'Unknown error');
        this.isLoading = false;
      }
    });
  }

  /**
   * Select a session as current
   */
  selectSession(sessionId: string): void {
    this.currentSessionId = sessionId;
    this.prologService.setCurrentSession(sessionId);
    this.sessionSelected.emit(sessionId);
  }

  /**
   * Check if session is currently selected
   */
  isCurrentSession(sessionId: string): boolean {
    return this.currentSessionId === sessionId;
  }

  /**
   * Generate a unique session ID
   */
  generateSessionId(): void {
    const timestamp = Date.now().toString(36);
    const random = Math.random().toString(36).substring(2, 6);
    this.newSessionId = `session-${timestamp}-${random}`;
  }

  /**
   * Clear messages after timeout
   */
  private clearMessages(): void {
    setTimeout(() => {
      this.successMessage = null;
    }, 5000);
  }
}
