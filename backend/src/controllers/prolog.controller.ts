/**
 * Prolog Controller
 * 
 * HTTP request handlers for Prolog operations.
 * Delegates Prolog execution to MCP Prolog Server via client.
 */

import { Request, Response } from 'express';
import { mcpPrologClient } from '../services/mcp-prolog-client';
import { templateService } from '../services/template-service';
import { createRule, getRules, deleteRule } from '../models/rule.model';
import { logger } from '../utils/logger';
import type { QueryRequest, RuleInput } from '../types';

// Session tracking for backwards compatibility
let currentSessionId: string | null = null;

/**
 * Save a new rule to the database
 */
export async function saveRule(req: Request, res: Response): Promise<void> {
  try {
    const ruleInput: RuleInput = req.body;
    const result = await createRule(ruleInput);
    res.status(201).json(result);
  } catch (error: any) {
    logger.error('Error saving rule', { error: error.message });
    res.status(500).json({ error: 'Error saving rule' });
  }
}

/**
 * Get rules from database, optionally filtered by app
 */
export async function getRulesHandler(req: Request, res: Response): Promise<void> {
  try {
    const { id } = req.params; // 'id' is actually app filter (legacy API)
    const rules = await getRules(id);
    res.json(rules);
  } catch (error: any) {
    logger.error('Error getting rules', { error: error.message });
    res.status(500).json({ error: 'Error getting rules' });
  }
}

/**
 * Delete a rule by ID
 */
export async function deleteRuleHandler(req: Request, res: Response): Promise<void> {
  try {
    const id = parseInt(req.params.id, 10);
    await deleteRule(id);
    res.status(204).send();
  } catch (error: any) {
    logger.error('Error deleting rule', { error: error.message });
    res.status(500).json({ error: 'Error deleting rule' });
  }
}

/**
 * Execute a Prolog query via MCP server
 */
export async function runRule(req: Request, res: Response): Promise<void> {
  try {
    const { text, sessionId }: QueryRequest = req.body;
    
    // Use provided sessionId or current session
    const sid = sessionId || currentSessionId;
    
    if (!sid) {
      res.status(400).json({ 
        error: 'No active session. Load a template first or provide sessionId.' 
      });
      return;
    }

    // Ensure MCP client is connected
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.query(sid, text);
    
    res.json({
      success: true,
      status: result.status,
      payload: result.payload,
      query: text,
      count: result.payload?.length || 0,
    });
  } catch (error: any) {
    logger.error('Error running query', { error: error.message });
    res.status(500).json({ success: false, status: 500, payload: [], error: error.message });
  }
}

/**
 * Get available SDK templates
 */
export async function getSdkTemplates(req: Request, res: Response): Promise<void> {
  try {
    const templates = await templateService.getSdkTemplates();
    res.json(templates);
  } catch (error: any) {
    logger.error('Error getting SDK templates', { error: error.message });
    res.status(500).json({ error: 'Error getting SDK templates' });
  }
}

/**
 * Get template content and initialize MCP session
 */
export async function getTemplateContent(req: Request, res: Response): Promise<void> {
  try {
    const { templateName } = req.params;
    const content = await templateService.getTemplateContent(templateName);

    if (!content) {
      res.status(404).json({ error: 'Template not found' });
      return;
    }

    // Create MCP session for this template
    try {
      if (!mcpPrologClient.isConnected()) {
        await mcpPrologClient.connect();
      }

      // Generate session ID based on template
      const sessionId = `session_${templateName}_${Date.now()}`;
      const obraId = templateName;
      
      await mcpPrologClient.createSession(sessionId, obraId);
      currentSessionId = sessionId;
      
      logger.info('Created MCP session for template', { sessionId, templateName });
    } catch (mcpError: any) {
      logger.warn('MCP session creation failed, template loaded locally only', { 
        error: mcpError.message 
      });
    }

    res.json({ content });
  } catch (error: any) {
    logger.error('Error getting template content', { error: error.message });
    res.status(500).json({ error: 'Error getting template content' });
  }
}

/**
 * Save user application
 */
export async function saveUserApp(req: Request, res: Response): Promise<void> {
  try {
    const { appName, content } = req.body;
    const success = await templateService.saveUserApp(appName, content);
    
    if (success) {
      res.status(201).json({ message: 'App saved successfully' });
    } else {
      res.status(500).json({ error: 'Error saving app' });
    }
  } catch (error: any) {
    logger.error('Error saving user app', { error: error.message });
    res.status(500).json({ error: 'Error saving user app' });
  }
}

// ============================================
// New MCP Session Endpoints
// ============================================

/**
 * Create a new Prolog session
 */
export async function createSession(req: Request, res: Response): Promise<void> {
  try {
    const { sessionId, obraId } = req.body;
    
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.createSession(sessionId, obraId);
    
    if (result.success) {
      currentSessionId = sessionId;
    }
    
    res.json(result);
  } catch (error: any) {
    logger.error('Error creating session', { error: error.message });
    res.status(500).json({ success: false, error: error.message });
  }
}

/**
 * List all active sessions
 */
export async function listSessions(req: Request, res: Response): Promise<void> {
  try {
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.listSessions();
    res.json(result);
  } catch (error: any) {
    logger.error('Error listing sessions', { error: error.message });
    res.status(500).json({ success: false, error: error.message });
  }
}

/**
 * Destroy a session
 */
export async function destroySession(req: Request, res: Response): Promise<void> {
  try {
    const { sessionId } = req.params;
    
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.destroySession(sessionId);
    
    if (result.success && currentSessionId === sessionId) {
      currentSessionId = null;
    }
    
    res.json(result);
  } catch (error: any) {
    logger.error('Error destroying session', { error: error.message });
    res.status(500).json({ success: false, error: error.message });
  }
}

/**
 * Assert a fact
 */
export async function assertFact(req: Request, res: Response): Promise<void> {
  try {
    const { sessionId, fact } = req.body;
    const sid = sessionId || currentSessionId;
    
    if (!sid) {
      res.status(400).json({ success: false, error: 'No active session' });
      return;
    }

    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.assertFact(sid, fact);
    res.json(result);
  } catch (error: any) {
    logger.error('Error asserting fact', { error: error.message });
    res.status(500).json({ success: false, error: error.message });
  }
}

/**
 * Consult a Prolog file
 */
export async function consultFile(req: Request, res: Response): Promise<void> {
  try {
    const { sessionId, filePath } = req.body;
    const sid = sessionId || currentSessionId;
    
    if (!sid) {
      res.status(400).json({ success: false, error: 'No active session' });
      return;
    }

    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.consultFile(sid, filePath);
    res.json(result);
  } catch (error: any) {
    logger.error('Error consulting file', { error: error.message });
    res.status(500).json({ success: false, error: error.message });
  }
}

/**
 * Get MCP templates catalog
 */
export async function getMcpTemplates(req: Request, res: Response): Promise<void> {
  try {
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    const result = await mcpPrologClient.getTemplates();
    res.json(result);
  } catch (error: any) {
    logger.error('Error getting MCP templates', { error: error.message });
    res.status(500).json({ templates: [], message: error.message });
  }
}
