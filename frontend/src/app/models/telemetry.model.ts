/**
 * Telemetry models - re-exported from @alephscript/mcp-core-sdk/browser
 */
export type {
  Telemetry,
  TelemetryInput,
  TelemetryResult,
  TelemetryStatus,
} from '@alephscript/mcp-core-sdk/browser';

/**
 * MCP Event types - for monitoring MCP operations
 */
export type {
  MCPEvent,
} from '@alephscript/mcp-core-sdk/browser';

export {
  MCPEventType,
} from '@alephscript/mcp-core-sdk/browser';

/** Frontend telemetry record (mirrors backend MCPTelemetryRecord) */
export interface MCPTelemetryRecord {
  timestamp: number;
  event: import('@alephscript/mcp-core-sdk/browser').MCPEvent;
  duration?: number;
  metadata?: Record<string, unknown>;
}

/** Telemetry statistics from backend */
export interface MCPTelemetryStats {
  totalEvents: number;
  eventsByType: Record<string, number>;
  eventsByServer: Record<string, number>;
  avgDuration: number;
  errorCount: number;
}
