/**
 * MCP Client Pool Service
 * 
 * Multi-server MCP client with health checks, auto-reconnect, and event emission.
 * Connects to multiple MCP servers (Prolog, DevOps, Wiki, etc.) via a single pool.
 * 
 * @module @alephscript/prolog-editor-backend/services/mcp-pool
 */

import { MCPClientPool, type IMCPDriver, type MCPServerTransportConfig } from "@alephscript/mcp-core-sdk/client";
import type { MCPEvent } from "@alephscript/mcp-core-sdk/types";
import { logger } from '../utils/logger';

/** Default server configurations */
const DEFAULT_SERVERS: MCPServerTransportConfig[] = [
  {
    id: 'prolog',
    name: 'Prolog MCP Server',
    url: process.env.MCP_PROLOG_SERVER_URL || 'http://localhost:3006',
    timeout: 30000,
  },
  {
    id: 'devops',
    name: 'DevOps MCP Server',
    url: process.env.MCP_DEVOPS_SERVER_URL || 'http://localhost:3003',
    timeout: 30000,
  },
];

/** Pool configuration */
export interface MCPPoolConfig {
  /** Server configurations (defaults to Prolog + DevOps) */
  servers?: MCPServerTransportConfig[];
  /** Health check interval in ms (default: 30000) */
  healthCheckInterval?: number;
  /** Enable verbose logging */
  verbose?: boolean;
}

/** Event handlers for MCP events */
export type MCPEventHandler = (event: MCPEvent) => void;

/**
 * MCPPoolService - Singleton wrapper around MCPClientPool
 * 
 * Features:
 * - Multi-server connection pooling
 * - Health checks with auto-reconnect
 * - Event emission for telemetry integration
 * - Request mutex to prevent race conditions
 */
class MCPPoolService {
  private pool: MCPClientPool;
  private eventHandlers: MCPEventHandler[] = [];
  private initialized = false;
  private servers: MCPServerTransportConfig[];

  constructor(config: MCPPoolConfig = {}) {
    this.servers = config.servers || DEFAULT_SERVERS;
    
    this.pool = new MCPClientPool({
      healthCheckInterval: config.healthCheckInterval || 30000,
    });

    // Forward events to registered handlers
    this.pool.on('mcp-event', (event: MCPEvent) => {
      this.eventHandlers.forEach(handler => {
        try {
          handler(event);
        } catch (err) {
          logger.error('Event handler error', { error: err });
        }
      });
    });

    // Log connection events
    this.pool.on('server-connected', (serverId: string) => {
      logger.info(`MCP Server connected: ${serverId}`);
    });

    this.pool.on('server-disconnected', (serverId: string) => {
      logger.warn(`MCP Server disconnected: ${serverId}`);
    });

    this.pool.on('mcp-sync-error', (data: { serverId: string; error: Error }) => {
      logger.error(`MCP Sync error on ${data.serverId}`, { error: data.error.message });
    });
  }

  /**
   * Initialize all server connections
   */
  async initialize(): Promise<void> {
    if (this.initialized) return;
    
    try {
      // Add all configured servers
      for (const server of this.servers) {
        await this.pool.addServer(server);
      }
      this.initialized = true;
      logger.info('MCP Pool initialized', { 
        servers: this.pool.getServers().map(s => s.id) 
      });
    } catch (error) {
      logger.error('MCP Pool initialization failed', { error });
      throw error;
    }
  }

  /**
   * Get the underlying pool (implements IMCPDriver)
   */
  getPool(): IMCPDriver {
    return this.pool;
  }

  /**
   * Register an event handler for MCPEvents
   */
  onEvent(handler: MCPEventHandler): void {
    this.eventHandlers.push(handler);
  }

  /**
   * Remove an event handler
   */
  offEvent(handler: MCPEventHandler): void {
    const index = this.eventHandlers.indexOf(handler);
    if (index !== -1) {
      this.eventHandlers.splice(index, 1);
    }
  }

  /**
   * Execute a tool on a specific server
   */
  async callTool<T = any>(
    serverId: string,
    toolName: string,
    args: Record<string, unknown>
  ): Promise<T> {
    if (!this.initialized) {
      await this.initialize();
    }
    return this.pool.executeTool(serverId, toolName, args) as Promise<T>;
  }

  /**
   * Execute a tool on the Prolog server (convenience method)
   */
  async callPrologTool<T = any>(
    toolName: string,
    args: Record<string, unknown>
  ): Promise<T> {
    return this.callTool<T>('prolog', toolName, args);
  }

  /**
   * Execute a tool on the DevOps server (convenience method)
   */
  async callDevOpsTool<T = any>(
    toolName: string,
    args: Record<string, unknown>
  ): Promise<T> {
    return this.callTool<T>('devops', toolName, args);
  }

  /**
   * Check health of all servers
   */
  async healthCheckAll(): Promise<Map<string, boolean>> {
    const results = new Map<string, boolean>();
    for (const server of this.servers) {
      const isHealthy = await this.pool.healthCheck(server.id);
      results.set(server.id, isHealthy);
    }
    return results;
  }

  /**
   * Check if a specific server is connected
   */
  isServerConnected(serverId: string): boolean {
    return this.pool.isConnected();
  }

  /**
   * Get all server configurations
   */
  getServers(): MCPServerTransportConfig[] {
    return this.pool.getServers();
  }

  /**
   * Disconnect all servers
   */
  async disconnect(): Promise<void> {
    await this.pool.close();
    this.initialized = false;
    logger.info('MCP Pool disconnected');
  }

  /**
   * Reconnect a specific server
   */
  async reconnectServer(serverId: string): Promise<void> {
    await this.pool.reconnectClient(serverId);
    logger.info(`MCP Server reconnected: ${serverId}`);
  }
}

// Singleton instance
export const mcpPoolService = new MCPPoolService();

// Export class for testing/custom instances
export { MCPPoolService };
