/**
 * PrologEditor Backend - REST API Gateway
 * 
 * This is a refactored TypeScript version that:
 * - Does NOT include direct Prolog engine (swipl) - delegated to MCP Server
 * - Acts as REST API gateway between Angular frontend and MCP Prolog Server
 * - Maintains backwards compatibility with existing frontend API
 * - Adds new session-based MCP endpoints
 * 
 * Architecture:
 *   Angular Frontend <--HTTP--> This Backend <--MCP--> MCPPrologServer
 * 
 * @module @alephscript/prolog-editor-backend
 */

import express, { Application, Request, Response, NextFunction } from 'express';
import cors from 'cors';
import winston from 'winston';
import { apiRoutes, telemetryRoutes } from './routes';
import { initRulesTable } from './models/rule.model';
import { mcpPrologClient } from './services/mcp-prolog-client';

// Create logger directly to avoid import issues
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    winston.format.printf(({ level, message, timestamp }) => `${timestamp} [${level}]: ${message}`)
  ),
  transports: [
    new winston.transports.Console({
      format: winston.format.combine(
        winston.format.colorize(),
        winston.format.printf(({ level, message, timestamp }) => `${timestamp} [${level}]: ${message}`)
      ),
    }),
  ],
});

const app: Application = express();
const port = process.env.PORT || 8000;

// ============================================
// Middleware
// ============================================

app.use(cors());
app.use(express.json());

// Request logging
app.use((req: Request, res: Response, next: NextFunction) => {
  logger.debug(`${req.method} ${req.path}`, { 
    query: req.query, 
    body: req.method !== 'GET' ? req.body : undefined 
  });
  next();
});

// ============================================
// Routes
// ============================================

app.use('/api', apiRoutes);
app.use('/api/telemetry', telemetryRoutes);

// Health check endpoint
app.get('/health', (req: Request, res: Response) => {
  res.json({
    status: 'healthy',
    version: '2.0.0',
    mcpConnected: mcpPrologClient.isConnected(),
    timestamp: new Date().toISOString(),
  });
});

// ============================================
// Error handling
// ============================================

app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
  logger.error(`Unhandled error: ${err.message}`);
  if (err.stack) {
    logger.error(`Stack: ${err.stack}`);
  }
  res.status(500).json({ error: 'Something went wrong!' });
});

// ============================================
// Startup
// ============================================

async function startServer(): Promise<void> {
  try {
    // Initialize database
    await initRulesTable();
    logger.info('Database initialized');

    // Optionally connect to MCP server on startup
    // (can also connect lazily on first request)
    try {
      // await mcpPrologClient.connect();
      logger.info('MCP client ready (lazy connection)');
    } catch (mcpError: any) {
      logger.warn('MCP server not available on startup', { error: mcpError.message });
    }

    // Start HTTP server
    app.listen(port, () => {
      logger.info(`PrologEditor Backend listening on http://0.0.0.0:${port}`);
      logger.info('API endpoints:');
      logger.info('  GET  /health - Health check');
      logger.info('  GET  /api/rules - List rules');
      logger.info('  POST /api/rules - Create rule');
      logger.info('  POST /api/run-rule - Execute Prolog query');
      logger.info('  GET  /api/sdk-templates - List templates');
      logger.info('  GET  /api/template/:name - Load template');
      logger.info('  POST /api/sessions - Create MCP session');
      logger.info('  GET  /api/sessions - List MCP sessions');
    });
  } catch (error: any) {
    logger.error('Failed to start server', { error: error.message });
    process.exit(1);
  }
}

// Handle graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, shutting down...');
  await mcpPrologClient.disconnect();
  process.exit(0);
});

process.on('SIGINT', async () => {
  logger.info('SIGINT received, shutting down...');
  await mcpPrologClient.disconnect();
  process.exit(0);
});

// Start the server
startServer();

export default app;
