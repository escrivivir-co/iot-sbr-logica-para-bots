import { Component, OnInit, Input } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

/**
 * Knowledge Base Component
 * 
 * Allows interactive KB operations: assert facts and consult files.
 * Tools covered: prolog_assert_fact, prolog_consult_file
 * 
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-knowledge-base',
  templateUrl: './knowledge-base.component.html',
  styleUrls: ['./knowledge-base.component.css']
})
export class KnowledgeBaseComponent implements OnInit {
  @Input() sessionId: string | null = null;
  
  // Assert Fact
  factText: string = '';
  assertResult: string | null = null;
  assertError: string | null = null;
  isAsserting: boolean = false;
  
  // Consult File
  filePath: string = '';
  consultResult: string | null = null;
  consultError: string | null = null;
  isConsulting: boolean = false;
  
  // Consulted files history
  consultedFiles: string[] = [];
  
  // Syntax hints
  readonly FACT_EXAMPLES = [
    'likes(mary, wine).',
    'parent(tom, bob).',
    'person(john, 25, developer).',
    'has_skill(alice, [python, typescript]).'
  ];

  constructor(private prologService: PrologService) {}

  ngOnInit(): void {
    // Get current session from service
    if (!this.sessionId) {
      this.sessionId = this.prologService.getCurrentSession();
    }
  }

  /**
   * Assert a fact to the knowledge base
   */
  assertFact(): void {
    const validation = this.validatePrologSyntax(this.factText);
    if (!validation.valid) {
      this.assertError = validation.error || 'Invalid syntax';
      return;
    }
    
    this.isAsserting = true;
    this.assertError = null;
    this.assertResult = null;
    
    this.prologService.assertFact(this.factText, this.sessionId || undefined).subscribe({
      next: (response) => {
        this.assertResult = `Fact asserted successfully: ${this.factText}`;
        this.factText = '';
        this.isAsserting = false;
      },
      error: (err) => {
        this.assertError = 'Error asserting fact: ' + (err.error?.error || err.message || 'Unknown error');
        this.isAsserting = false;
      }
    });
  }

  /**
   * Consult (load) a Prolog file
   */
  consultFile(): void {
    if (!this.filePath.trim()) {
      this.consultError = 'File path is required';
      return;
    }
    
    this.isConsulting = true;
    this.consultError = null;
    this.consultResult = null;
    
    this.prologService.consultFile(this.filePath, this.sessionId || undefined).subscribe({
      next: (response) => {
        this.consultResult = `File consulted successfully: ${this.filePath}`;
        
        // Add to history if not already present
        if (!this.consultedFiles.includes(this.filePath)) {
          this.consultedFiles.push(this.filePath);
        }
        
        this.filePath = '';
        this.isConsulting = false;
      },
      error: (err) => {
        this.consultError = 'Error consulting file: ' + (err.error?.error || err.message || 'Unknown error');
        this.isConsulting = false;
      }
    });
  }

  /**
   * Basic client-side Prolog syntax validation
   */
  validatePrologSyntax(text: string): { valid: boolean; error?: string } {
    const trimmed = text.trim();
    
    if (!trimmed) {
      return { valid: false, error: 'Fact cannot be empty' };
    }
    
    // Must end with period
    if (!trimmed.endsWith('.')) {
      return { valid: false, error: 'Prolog facts must end with a period (.)' };
    }
    
    // Basic functor pattern: name(args) or atom
    const factPattern = /^[a-z_][a-zA-Z0-9_]*(\([^)]+\))?\.$/;
    const rulePattern = /^[a-z_][a-zA-Z0-9_]*(\([^)]*\))?\s*:-\s*.+\.$/;
    
    if (!factPattern.test(trimmed) && !rulePattern.test(trimmed)) {
      // More lenient check - just ensure it starts with lowercase and has balanced parens
      const openParens = (trimmed.match(/\(/g) || []).length;
      const closeParens = (trimmed.match(/\)/g) || []).length;
      
      if (openParens !== closeParens) {
        return { valid: false, error: 'Unbalanced parentheses' };
      }
      
      if (!/^[a-z_]/.test(trimmed)) {
        return { valid: false, error: 'Predicates must start with lowercase letter or underscore' };
      }
    }
    
    return { valid: true };
  }

  /**
   * Insert example fact
   */
  insertExample(example: string): void {
    this.factText = example;
  }

  /**
   * Re-consult a file from history
   */
  reconsultFile(path: string): void {
    this.filePath = path;
    this.consultFile();
  }

  /**
   * Clear consulted files history
   */
  clearHistory(): void {
    this.consultedFiles = [];
  }

  /**
   * Check if session is available
   */
  hasSession(): boolean {
    return !!(this.sessionId || this.prologService.getCurrentSession());
  }
}
