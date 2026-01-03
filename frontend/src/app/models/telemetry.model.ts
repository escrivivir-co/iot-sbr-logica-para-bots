/**
 * Telemetry models - re-exported from @alephscript/mcp-core-sdk
 */
export type {
  Telemetry,
  TelemetryInput,
  TelemetryResult,
  TelemetryStatus,
} from '@alephscript/mcp-core-sdk/types/prolog';

/**
 * MCP Event types - for monitoring MCP operations
 */
export type {
  MCPEvent,
} from '@alephscript/mcp-core-sdk/types';

export {
  MCPEventType,
} from '@alephscript/mcp-core-sdk/types';

/** Frontend telemetry record (mirrors backend MCPTelemetryRecord) */
export interface MCPTelemetryRecord {
  timestamp: number;
  event: import('@alephscript/mcp-core-sdk/types').MCPEvent;
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
