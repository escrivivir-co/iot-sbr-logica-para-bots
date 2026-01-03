/**
 * Shared Prolog Types - Re-exported from @alephscript/mcp-core-sdk
 * 
 * This file re-exports all Prolog types from the shared SDK.
 * 
 * @module @alephscript/prolog-editor-backend/types/prolog
 */

export type {
  // Session Types
  PrologSession,
  CreateSessionRequest,
  CreateSessionResponse,
  SessionResponse, // Alias for backwards compatibility
  ListSessionsResponse,
  
  // Query Types
  QueryRequest,
  QueryResponse,
  QueryResult,
  
  // Rule Types
  Rule,
  RuleInput,
  RuleCreatedResponse,
  
  // Template Types
  Template,
  TemplateContentResponse,
  UserAppInput,
  TemplatesCatalog,
  
  // Assert/Consult Types
  AssertFactRequest,
  AssertFactResponse,
  ConsultFileRequest,
  ConsultFileResponse,
  
  // Telemetry Types
  Telemetry,
  TelemetryInput,
  TelemetryResult,
  TelemetryStatus,
  
  // IoT/MQTT Types
  SensorData,
  AlertData,
  AlertSeverity,
  CommandData,
  
  // API Types
  ApiResponse,
  ApiError,
} from '@alephscript/mcp-core-sdk/types';

export { PrologErrorType } from '@alephscript/mcp-core-sdk/types';
