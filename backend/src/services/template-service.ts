/**
 * Template Service
 * 
 * Manages Prolog SDK templates. In the refactored architecture,
 * templates are served locally but execution delegates to MCP Prolog Server.
 * 
 * Template sources (in priority order):
 * 1. MCP Presets packs (.pack.json)
 * 2. ARCHIVO/PLUGINS/PROLOG_EDITOR/templates
 * 3. Built-in default templates
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import { logger } from '../utils/logger';
import type { Template, TemplateContentResponse } from '../types';

// Path to MCP Presets packs (primary source)
const MCP_PACKS_PATH = path.resolve(__dirname, '../../../../../.github/plugins/mcp-presets/packs');

// Path to ARCHIVO-based templates
const ARCHIVO_TEMPLATES_PATH = path.resolve(__dirname, '../../../../../ARCHIVO/PLUGINS/PROLOG_EDITOR/templates');

// Path to Teatro ELENCO brains (Lucas, etc.)
const ELENCO_BRAINS_PATH = path.resolve(__dirname, '../../../../../ARCHIVO/DISCO/TALLER/ELENCO');

// Default templates path (primary source = MCP Presets packs)
const TEMPLATES_PATH = MCP_PACKS_PATH;

export class TemplateService {
  private templatesPath: string;

  constructor(templatesPath?: string) {
    this.templatesPath = templatesPath || TEMPLATES_PATH;
  }

  /**
   * Get all available SDK templates
   */
  async getSdkTemplates(): Promise<Template[]> {
    try {
      // Try primary path first
      let files: string[] = [];
      try {
        files = await fs.readdir(this.templatesPath);
      } catch {
        // Try ARCHIVO path as fallback
        try {
          await fs.access(ARCHIVO_TEMPLATES_PATH);
          files = await fs.readdir(ARCHIVO_TEMPLATES_PATH);
          this.templatesPath = ARCHIVO_TEMPLATES_PATH;
        } catch {
          logger.warn('No templates directory found');
          return this.getDefaultTemplates();
        }
      }

      // Support both .template and .pack.json files
      const templateFiles = files.filter(file => 
        file.endsWith('.template') || file.endsWith('.pack.json')
      );
      const templates: Template[] = [];

      for (const templateFile of templateFiles) {
        try {
          const data = await fs.readFile(
            path.join(this.templatesPath, templateFile),
            'utf8'
          );
          const rawData = JSON.parse(data);
          
          // Handle different JSON formats:
          // 1. .pack.json format: has 'id', 'name', 'description' at root level
          // 2. .template format: has 'name', 'description', 'files', 'exports'
          // 3. Legacy with 'pack' wrapper
          let template: Template;
          
          if (rawData.pack) {
            // Legacy pack wrapper
            template = {
              name: rawData.pack.name || templateFile.replace(/\.(template|pack\.json)$/, ''),
              description: rawData.pack.description || '',
              files: rawData.pack.files || [],
              exports: rawData.pack.exports || [],
              main: rawData.pack.main,
            };
          } else if (rawData.id || rawData.mcpServer) {
            // Modern .pack.json format
            template = {
              name: rawData.name || rawData.id || templateFile.replace(/\.pack\.json$/, ''),
              description: rawData.description || '',
              files: rawData.files || [],
              exports: rawData.exports || [],
              main: rawData.main,
            };
          } else {
            // Standard .template format
            template = rawData as Template;
          }
          
          templates.push(template);
          logger.debug(`Loaded template: ${templateFile}`, { name: template.name });
        } catch (error) {
          logger.warn(`Failed to parse template ${templateFile}`, { error });
        }
      }

      return templates.length > 0 ? templates : this.getDefaultTemplates();
    } catch (error) {
      logger.error('Error reading SDK templates', { error });
      return this.getDefaultTemplates();
    }
  }

  /**
   * Get content of a specific template
   */
  async getTemplateContent(templateName: string): Promise<string | null> {
    try {
      // Load template metadata
      const templatePath = path.join(this.templatesPath, `${templateName}.template`);
      const metadataStr = await fs.readFile(templatePath, 'utf8');
      const metadata = JSON.parse(metadataStr) as Template;

      // Load main Prolog file
      const mainFile = metadata.main || templateName;
      const plPath = path.join(this.templatesPath, mainFile, 'app.pl');
      
      try {
        const content = await fs.readFile(plPath, 'utf8');
        return content;
      } catch {
        // Try alternative structure
        const altPath = path.join(this.templatesPath, `${templateName}.pl`);
        const content = await fs.readFile(altPath, 'utf8');
        return content;
      }
    } catch (error) {
      logger.error(`Error reading template ${templateName}`, { error });
      return null;
    }
  }

  /**
   * Save user application
   */
  async saveUserApp(appName: string, content: string): Promise<boolean> {
    try {
      const filePath = path.join(this.templatesPath, `${appName}.pl`);
      await fs.writeFile(filePath, content, 'utf8');
      logger.info(`Saved user app: ${appName}`);
      return true;
    } catch (error) {
      logger.error(`Error saving user app ${appName}`, { error });
      return false;
    }
  }

  /**
   * Get default templates when no file-based templates are available
   */
  private getDefaultTemplates(): Template[] {
    return [
      {
        name: 'iot-app',
        description: 'IoT Application Template with sensor rules',
        files: ['app.pl', 'sdk/'],
        exports: ['process/1', 'alert/1'],
      },
      {
        name: 'state-machine',
        description: 'State Machine Template for FSM modeling',
        files: ['app.pl'],
        exports: ['transition/3', 'state/1'],
      },
      {
        name: 'simu',
        description: 'Simulation Rules Template',
        files: ['app.pl'],
        exports: ['simulate/2', 'step/1'],
      },
    ];
  }

  /**
   * Get templates path (for debugging)
   */
  getTemplatesPath(): string {
    return this.templatesPath;
  }
}

// Default singleton instance
export const templateService = new TemplateService();
