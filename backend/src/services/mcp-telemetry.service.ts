/**
 * MCP Telemetry Service
 * 
 * Integrates MCPEvent emission with telemetry logging.
 * Registers as event handler on MCPPoolService.
 * 
 * @module @alephscript/prolog-editor-backend/services/mcp-telemetry
 */

import { MCPEventType, type MCPEvent } from "@alephscript/mcp-core-sdk/types";
import { mcpPoolService } from './mcp-pool.service';
import { logger } from '../utils/logger';

/** Telemetry record for MCP operations */
export interface MCPTelemetryRecord {
  timestamp: number;
  event: MCPEvent;
  duration?: number;
  metadata?: Record<string, unknown>;
}

/** Telemetry storage (in-memory, replace with DB in production) */
const telemetryStore: MCPTelemetryRecord[] = [];
const MAX_RECORDS = 1000;

/** Event timing tracker */
const eventTimers = new Map<string, number>();

/**
 * MCPTelemetryService - Tracks and logs all MCP operations
 */
class MCPTelemetryService {
  private enabled = true;

  constructor() {
    // Register as event handler on pool service
    mcpPoolService.onEvent(this.handleEvent.bind(this));
  }

  /**
   * Handle incoming MCP events
   */
  private handleEvent(event: MCPEvent): void {
    if (!this.enabled) return;

    const record: MCPTelemetryRecord = {
      timestamp: Date.now(),
      event,
    };

    // Calculate duration for completed operations
    const timerKey = `${event.serverId}:${event.action}`;
    if (event.type === MCPEventType.TOOL_EXECUTED || event.type === MCPEventType.RESOURCE_RETRIEVED) {
      if (!eventTimers.has(timerKey)) {
        // Start timer
        eventTimers.set(timerKey, Date.now());
      } else {
        // End timer
        const startTime = eventTimers.get(timerKey)!;
        record.duration = Date.now() - startTime;
        eventTimers.delete(timerKey);
      }
    }

    // Store record
    this.addRecord(record);

    // Log based on event type
    this.logEvent(record);
  }

  /**
   * Add record to store (circular buffer)
   */
  private addRecord(record: MCPTelemetryRecord): void {
    telemetryStore.push(record);
    if (telemetryStore.length > MAX_RECORDS) {
      telemetryStore.shift();
    }
  }

  /**
   * Log event to console/file
   */
  private logEvent(record: MCPTelemetryRecord): void {
    const { event, duration } = record;
    const durationStr = duration ? ` (${duration}ms)` : '';

    switch (event.type) {
      case MCPEventType.SERVER_ERROR:
      case MCPEventType.SYNC_ERROR:
        logger.error(`MCP Error [${event.serverId}]: ${event.action}${durationStr}`, {
          error: event.error,
          data: event.data,
        });
        break;

      case MCPEventType.HEALTH_CHECK:
        logger.debug(`MCP Health [${event.serverId}]: ${event.action}${durationStr}`);
        break;

      case MCPEventType.TOOL_EXECUTED:
        logger.info(`MCP Tool [${event.serverId}]: ${event.action}${durationStr}`, {
          data: event.data,
        });
        break;

      case MCPEventType.RESOURCE_RETRIEVED:
        logger.info(`MCP Resource [${event.serverId}]: ${event.action}${durationStr}`);
        break;

      case MCPEventType.SERVER_CONNECTED:
      case MCPEventType.SERVER_DISCONNECTED:
      case MCPEventType.SERVER_RECONNECTED:
        logger.debug(`MCP State [${event.serverId}]: ${event.action}`);
        break;

      default:
        logger.debug(`MCP Event [${event.serverId}]: ${event.type}/${event.action}${durationStr}`);
    }
  }

  /**
   * Get all telemetry records
   */
  getRecords(): MCPTelemetryRecord[] {
    return [...telemetryStore];
  }

  /**
   * Get records filtered by server
   */
  getRecordsByServer(serverId: string): MCPTelemetryRecord[] {
    return telemetryStore.filter(r => r.event.serverId === serverId);
  }

  /**
   * Get records filtered by event type
   */
  getRecordsByType(type: MCPEventType): MCPTelemetryRecord[] {
    return telemetryStore.filter(r => r.event.type === type);
  }

  /**
   * Get recent records (last N)
   */
  getRecentRecords(count: number = 50): MCPTelemetryRecord[] {
    return telemetryStore.slice(-count);
  }

  /**
   * Get statistics
   */
  getStats(): {
    totalEvents: number;
    eventsByType: Record<string, number>;
    eventsByServer: Record<string, number>;
    avgDuration: number;
    errorCount: number;
  } {
    const eventsByType: Record<string, number> = {};
    const eventsByServer: Record<string, number> = {};
    let totalDuration = 0;
    let durationCount = 0;
    let errorCount = 0;

    for (const record of telemetryStore) {
      // By type
      const type = String(record.event.type);
      eventsByType[type] = (eventsByType[type] || 0) + 1;

      // By server
      const server = record.event.serverId;
      eventsByServer[server] = (eventsByServer[server] || 0) + 1;

      // Duration
      if (record.duration) {
        totalDuration += record.duration;
        durationCount++;
      }

      // Errors
      if (record.event.type === MCPEventType.SERVER_ERROR || record.event.type === MCPEventType.SYNC_ERROR) {
        errorCount++;
      }
    }

    return {
      totalEvents: telemetryStore.length,
      eventsByType,
      eventsByServer,
      avgDuration: durationCount > 0 ? totalDuration / durationCount : 0,
      errorCount,
    };
  }

  /**
   * Clear all records
   */
  clearRecords(): void {
    telemetryStore.length = 0;
    eventTimers.clear();
  }

  /**
   * Enable/disable telemetry
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled;
  }

  /**
   * Check if telemetry is enabled
   */
  isEnabled(): boolean {
    return this.enabled;
  }
}

// Singleton instance
export const mcpTelemetryService = new MCPTelemetryService();

// Export class for testing/custom instances
export { MCPTelemetryService };
