/**
 * MCP Prolog Client
 * 
 * Client to connect to the MCPPrologServer (mcp-mesh-sdk).
 * Provides a clean abstraction for Prolog operations without
 * direct dependency on swipl/swipl-stdio.
 * 
 * @module @alephscript/prolog-editor-backend/services/mcp-prolog-client
 */

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import type {
  QueryResponse,
  CreateSessionResponse,
  ListSessionsResponse,
  AssertFactResponse,
  ConsultFileResponse,
  TemplatesCatalog,
} from '../types/prolog.types';

// Simple inline logger to avoid import issues
const logger = {
  info: (msg: string, meta?: object) => console.log(`[INFO] ${new Date().toISOString()} ${msg}`, meta ? JSON.stringify(meta) : ''),
  error: (msg: string, meta?: object) => console.error(`[ERROR] ${new Date().toISOString()} ${msg}`, meta ? JSON.stringify(meta) : ''),
  warn: (msg: string, meta?: object) => console.warn(`[WARN] ${new Date().toISOString()} ${msg}`, meta ? JSON.stringify(meta) : ''),
  debug: (msg: string, meta?: object) => console.log(`[DEBUG] ${new Date().toISOString()} ${msg}`, meta ? JSON.stringify(meta) : ''),
};

// MCP SDK response types
interface MCPTextContent {
  type: 'text';
  text: string;
}

interface MCPToolResult {
  content: MCPTextContent[];
}

export interface MCPPrologClientConfig {
  /** Path to MCP Prolog server executable or npm script */
  serverCommand: string;
  /** Arguments for server command */
  serverArgs?: string[];
  /** Connection timeout in ms */
  timeout?: number;
}

const DEFAULT_CONFIG: MCPPrologClientConfig = {
  serverCommand: 'npx',
  serverArgs: ['ts-node', '../../MCPGallery/mcp-mesh-sdk/src/MCPPrologServer.ts'],
  timeout: 30000,
};

export class MCPPrologClient {
  private client: Client | null = null;
  private transport: StdioClientTransport | null = null;
  private config: MCPPrologClientConfig;
  private connected: boolean = false;

  constructor(config: Partial<MCPPrologClientConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  /**
   * Connect to the MCP Prolog Server
   */
  async connect(): Promise<void> {
    if (this.connected) {
      logger.info('MCP Prolog Client already connected');
      return;
    }

    try {
      this.client = new Client({
        name: 'prolog-editor-backend',
        version: '2.0.0',
      });

      this.transport = new StdioClientTransport({
        command: this.config.serverCommand,
        args: this.config.serverArgs || [],
      });

      await this.client.connect(this.transport);
      this.connected = true;
      logger.info('Connected to MCP Prolog Server');
    } catch (error: unknown) {
      const err = error instanceof Error ? { message: error.message } : { message: String(error) };
      logger.error('Failed to connect to MCP Prolog Server', err);
      throw error;
    }
  }

  /**
   * Disconnect from the MCP Prolog Server
   */
  async disconnect(): Promise<void> {
    if (this.transport) {
      await this.transport.close();
    }
    this.client = null;
    this.transport = null;
    this.connected = false;
    logger.info('Disconnected from MCP Prolog Server');
  }

  /**
   * Ensure client is connected
   */
  private ensureConnected(): void {
    if (!this.client || !this.connected) {
      throw new Error('MCP Prolog Client not connected');
    }
  }

  /**
   * Parse MCP tool result
   */
  private parseToolResult(result: unknown): string {
    const mcpResult = result as MCPToolResult;
    if (mcpResult.content && mcpResult.content[0]?.type === 'text') {
      return mcpResult.content[0].text;
    }
    throw new Error('Unexpected response format');
  }

  /**
   * Execute a Prolog query in a session
   */
  async query(sessionId: string, query: string): Promise<QueryResponse> {
    this.ensureConnected();
    
    try {
      const result = await this.client!.callTool({
        name: 'prolog_query',
        arguments: { sessionId, query },
      });

      const text = this.parseToolResult(result);
      const parsed = JSON.parse(text);
      return {
        success: parsed.success,
        status: parsed.success ? 200 : 500,
        payload: parsed.results || [],
        query: parsed.query,
        count: parsed.count,
        error: parsed.error,
      };
    } catch (error: any) {
      logger.error('Query failed', { sessionId, query, error: error.message });
      return {
        success: false,
        status: 500,
        payload: [],
        query,
        error: error.message,
      };
    }
  }

  /**
   * Create a new Prolog session
   */
  async createSession(sessionId: string, obraId: string): Promise<CreateSessionResponse> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_create_session',
        arguments: { sessionId, obraId },
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('Create session failed', { sessionId, obraId, error: error.message });
      return {
        success: false,
        error: error.message,
      };
    }
  }

  /**
   * Destroy a Prolog session
   */
  async destroySession(sessionId: string): Promise<{ success: boolean; message?: string; error?: string }> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_destroy_session',
        arguments: { sessionId },
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('Destroy session failed', { sessionId, error: error.message });
      return {
        success: false,
        error: error.message,
      };
    }
  }

  /**
   * List all active sessions
   */
  async listSessions(): Promise<ListSessionsResponse> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_list_sessions',
        arguments: {},
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('List sessions failed', { error: error.message });
      return {
        success: false,
        count: 0,
        sessions: [],
        error: error.message,
      };
    }
  }

  /**
   * Assert a fact into the knowledge base
   */
  async assertFact(sessionId: string, fact: string): Promise<AssertFactResponse> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_assert_fact',
        arguments: { sessionId, fact },
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('Assert fact failed', { sessionId, fact, error: error.message });
      return {
        success: false,
        fact,
        error: error.message,
      };
    }
  }

  /**
   * Consult a Prolog file
   */
  async consultFile(sessionId: string, filePath: string): Promise<ConsultFileResponse> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_consult_file',
        arguments: { sessionId, filePath },
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('Consult file failed', { sessionId, filePath, error: error.message });
      return {
        success: false,
        filePath,
        error: error.message,
      };
    }
  }

  /**
   * Get templates catalog
   */
  async getTemplates(): Promise<TemplatesCatalog> {
    this.ensureConnected();

    try {
      const result = await this.client!.callTool({
        name: 'prolog_get_templates',
        arguments: {},
      });

      const text = this.parseToolResult(result);
      return JSON.parse(text);
    } catch (error: any) {
      logger.error('Get templates failed', { error: error.message });
      return {
        templates: [],
        message: error.message,
      };
    }
  }

  /**
   * Check if connected
   */
  isConnected(): boolean {
    return this.connected;
  }
}

// Singleton instance for easy import
export const mcpPrologClient = new MCPPrologClient();
