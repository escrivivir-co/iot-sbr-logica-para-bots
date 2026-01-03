/**
 * API Routes
 * 
 * REST API endpoints for PrologEditor.
 * Maintains backwards compatibility with existing frontend while
 * adding new MCP session management endpoints.
 */

import { Router } from 'express';
import * as prologController from '../controllers/prolog.controller';

const router = Router();

// ============================================
// Legacy API (backwards compatible)
// ============================================

// Rules CRUD
router.post('/rules', prologController.saveRule);
router.get('/rules/:id', prologController.getRulesHandler);
router.get('/rules', prologController.getRulesHandler);
router.delete('/rules/:id', prologController.deleteRuleHandler);

// Query execution
router.post('/run-rule', prologController.runRule);

// Templates
router.get('/sdk-templates', prologController.getSdkTemplates);
router.get('/template/:templateName', prologController.getTemplateContent);
router.post('/user-app', prologController.saveUserApp);

// ============================================
// New MCP Session API
// ============================================

// Sessions
router.post('/sessions', prologController.createSession);
router.get('/sessions', prologController.listSessions);
router.delete('/sessions/:sessionId', prologController.destroySession);

// Prolog operations (with explicit session)
router.post('/assert', prologController.assertFact);
router.post('/consult', prologController.consultFile);

// MCP Templates (from AAIAGallery)
router.get('/mcp-templates', prologController.getMcpTemplates);

export default router;
