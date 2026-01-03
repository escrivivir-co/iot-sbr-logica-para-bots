import { Component, Input, Output, EventEmitter } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

/**
 * User App Save Dialog Component
 * 
 * Modal dialog to save custom Prolog applications.
 * Endpoint covered: POST /user-app
 * 
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-user-app-save-dialog',
  templateUrl: './user-app-save-dialog.component.html',
  styleUrls: ['./user-app-save-dialog.component.css']
})
export class UserAppSaveDialogComponent {
  @Input() content: string = '';
  @Input() isVisible: boolean = false;
  
  @Output() saved = new EventEmitter<{ name: string; content: string }>();
  @Output() closed = new EventEmitter<void>();
  
  appName: string = '';
  isSaving: boolean = false;
  error: string | null = null;
  successMessage: string | null = null;

  constructor(private prologService: PrologService) {}

  /**
   * Save the user application
   */
  save(): void {
    if (!this.appName.trim()) {
      this.error = 'Application name is required';
      return;
    }
    
    if (!this.content.trim()) {
      this.error = 'Content cannot be empty';
      return;
    }
    
    // Validate name format
    if (!/^[a-zA-Z][a-zA-Z0-9_-]*$/.test(this.appName)) {
      this.error = 'Name must start with a letter and contain only letters, numbers, hyphens, and underscores';
      return;
    }
    
    this.isSaving = true;
    this.error = null;
    
    this.prologService.saveUserApp(this.appName, this.content).subscribe({
      next: (response) => {
        this.successMessage = `Application "${this.appName}" saved successfully`;
        this.isSaving = false;
        this.saved.emit({ name: this.appName, content: this.content });
        
        // Auto-close after success
        setTimeout(() => {
          this.close();
        }, 1500);
      },
      error: (err) => {
        this.error = 'Error saving application: ' + (err.error?.error || err.message || 'Unknown error');
        this.isSaving = false;
      }
    });
  }

  /**
   * Close the dialog
   */
  close(): void {
    this.appName = '';
    this.error = null;
    this.successMessage = null;
    this.isVisible = false;
    this.closed.emit();
  }

  /**
   * Handle backdrop click
   */
  onBackdropClick(event: Event): void {
    if ((event.target as HTMLElement).classList.contains('modal-backdrop')) {
      this.close();
    }
  }

  /**
   * Get content preview (first N lines)
   */
  getContentPreview(): string {
    const lines = this.content.split('\n');
    const previewLines = lines.slice(0, 10);
    let preview = previewLines.join('\n');
    
    if (lines.length > 10) {
      preview += `\n... (${lines.length - 10} more lines)`;
    }
    
    return preview;
  }

  /**
   * Generate suggested name from content
   */
  suggestName(): void {
    // Try to extract predicate name from first line
    const firstLine = this.content.trim().split('\n')[0];
    const match = firstLine.match(/^([a-z_][a-zA-Z0-9_]*)/);
    
    if (match) {
      this.appName = match[1] + '_app';
    } else {
      const timestamp = Date.now().toString(36);
      this.appName = `my_app_${timestamp}`;
    }
  }
}
