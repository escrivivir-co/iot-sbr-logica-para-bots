/**
 * MCP Prolog Client - HTTP Streamable Transport
 * 
 * Client to connect to the MCPPrologServer (mcp-mesh-sdk) via HTTP.
 * Extends BaseMCPClient for shared connection management.
 * 
 * @module @alephscript/prolog-editor-backend/services/mcp-prolog-client
 */

import { BaseMCPClient } from "@alephscript/mcp-core-sdk/client";
import type {
  QueryResponse,
  CreateSessionResponse,
  ListSessionsResponse,
  AssertFactResponse,
  ConsultFileResponse,
  TemplatesCatalog,
} from '../types/prolog.types';

/** Configuration specific to MCPPrologClient */
export interface MCPPrologClientConfig {
  /** URL of the MCP Prolog server (HTTP endpoint) */
  serverUrl?: string;
  /** Connection timeout in ms */
  timeout?: number;
}

const DEFAULT_SERVER_URL = process.env.MCP_PROLOG_SERVER_URL || 'http://localhost:3006';

/**
 * MCP Prolog Client - extends BaseMCPClient with Prolog-specific operations
 */
export class MCPPrologClient extends BaseMCPClient {
  constructor(config: MCPPrologClientConfig = {}) {
    super({
      name: 'prolog-editor-backend',
      version: '2.0.0',
      serverUrl: config.serverUrl || DEFAULT_SERVER_URL,
      timeout: config.timeout,
    });
  }

  /**
   * Execute a Prolog query in a session
   */
  async query(sessionId: string, query: string): Promise<QueryResponse> {
    try {
      const parsed = await this.callTool<any>('prolog_query', { sessionId, query });
      return {
        success: parsed.success,
        status: parsed.success ? 200 : 500,
        payload: parsed.results || [],
        query: parsed.query,
        count: parsed.count,
        error: parsed.error,
      };
    } catch (error: any) {
      this.logger.error('Query failed', { sessionId, query, error: error.message });
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
    try {
      return await this.callTool<CreateSessionResponse>('prolog_create_session', { sessionId, obraId });
    } catch (error: any) {
      this.logger.error('Create session failed', { sessionId, obraId, error: error.message });
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
    try {
      return await this.callTool('prolog_destroy_session', { sessionId });
    } catch (error: any) {
      this.logger.error('Destroy session failed', { sessionId, error: error.message });
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
    try {
      return await this.callTool<ListSessionsResponse>('prolog_list_sessions', {});
    } catch (error: any) {
      this.logger.error('List sessions failed', { error: error.message });
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
    try {
      return await this.callTool<AssertFactResponse>('prolog_assert_fact', { sessionId, fact });
    } catch (error: any) {
      this.logger.error('Assert fact failed', { sessionId, fact, error: error.message });
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
    try {
      return await this.callTool<ConsultFileResponse>('prolog_consult_file', { sessionId, filePath });
    } catch (error: any) {
      this.logger.error('Consult file failed', { sessionId, filePath, error: error.message });
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
    try {
      return await this.callTool<TemplatesCatalog>('prolog_get_templates', {});
    } catch (error: any) {
      this.logger.error('Get templates failed', { error: error.message });
      return {
        templates: [],
        message: error.message,
      };
    }
  }
}

// Singleton instance for easy import
export const mcpPrologClient = new MCPPrologClient();
