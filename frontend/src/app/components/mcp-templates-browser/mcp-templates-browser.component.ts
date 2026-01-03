import { Component, OnInit, Output, EventEmitter } from '@angular/core';
import { PrologService } from '../../services/prolog.service';

interface McpTemplate {
  name: string;
  description?: string;
  category?: string;
  predicates?: string[];
}

/**
 * MCP Templates Browser Component
 * 
 * Explore and load MCP Prolog templates (different from SDK local templates).
 * Tool covered: prolog_get_templates
 * 
 * @epic PROLOG-UI-2.0.0
 */
@Component({
  selector: 'app-mcp-templates-browser',
  templateUrl: './mcp-templates-browser.component.html',
  styleUrls: ['./mcp-templates-browser.component.css']
})
export class McpTemplatesBrowserComponent implements OnInit {
  templates: McpTemplate[] = [];
  isLoading: boolean = false;
  error: string | null = null;
  selectedTemplate: McpTemplate | null = null;
  
  // Filter
  searchTerm: string = '';
  
  @Output() templateLoaded = new EventEmitter<McpTemplate>();

  constructor(private prologService: PrologService) {}

  ngOnInit(): void {
    this.loadTemplates();
  }

  /**
   * Load MCP templates from server
   */
  loadTemplates(): void {
    this.isLoading = true;
    this.error = null;
    
    this.prologService.getMcpTemplates().subscribe({
      next: (response) => {
        // Handle both array and object response
        if (Array.isArray(response)) {
          this.templates = response;
        } else if (response.templates) {
          this.templates = response.templates;
        } else {
          this.templates = [];
        }
        this.isLoading = false;
      },
      error: (err) => {
        this.error = 'Error loading templates: ' + (err.error?.error || err.message || 'Unknown error');
        this.isLoading = false;
      }
    });
  }

  /**
   * Filter templates by search term
   */
  get filteredTemplates(): McpTemplate[] {
    if (!this.searchTerm.trim()) {
      return this.templates;
    }
    
    const term = this.searchTerm.toLowerCase();
    return this.templates.filter(t => 
      t.name.toLowerCase().includes(term) ||
      (t.description && t.description.toLowerCase().includes(term)) ||
      (t.category && t.category.toLowerCase().includes(term))
    );
  }

  /**
   * Select a template for preview
   */
  selectTemplate(template: McpTemplate): void {
    this.selectedTemplate = template;
  }

  /**
   * Load selected template (creates session automatically)
   */
  loadTemplate(template: McpTemplate): void {
    // Get template content which auto-creates session
    this.prologService.getTemplateContent(template.name).subscribe({
      next: (response) => {
        this.templateLoaded.emit(template);
      },
      error: (err) => {
        this.error = 'Error loading template: ' + (err.error?.error || err.message);
      }
    });
  }

  /**
   * Check if template is selected
   */
  isSelected(template: McpTemplate): boolean {
    return this.selectedTemplate?.name === template.name;
  }

  /**
   * Get category badge color
   */
  getCategoryColor(category?: string): string {
    const colors: { [key: string]: string } = {
      'reasoning': 'bg-primary',
      'family': 'bg-success',
      'math': 'bg-info',
      'games': 'bg-warning',
      'utils': 'bg-secondary',
      'teatro': 'bg-danger'
    };
    return colors[category?.toLowerCase() || ''] || 'bg-secondary';
  }
}
