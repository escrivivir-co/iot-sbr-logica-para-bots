/**
 * Telemetry Controller
 * 
 * Handles IoT telemetry processing via MCP Prolog Server.
 */

import { Request, Response } from 'express';
import { mcpPrologClient } from '../services/mcp-prolog-client';
import { logger } from '../utils/logger';
import type { TelemetryInput, TelemetryResult, TelemetryStatus } from '../types';

// Current session for telemetry operations
let telemetrySessionId: string | null = null;

/**
 * Convert telemetry data to Prolog facts
 */
function telemetryToPrologFacts(telemetry: { sensor: string; value: string | number }): string[] {
  const facts: string[] = [];
  const { sensor, value } = telemetry;
  
  // Generate fact: sensor_value(sensor_name, value)
  if (typeof value === 'number') {
    facts.push(`sensor_value('${sensor}', ${value})`);
  } else {
    facts.push(`sensor_value('${sensor}', '${value}')`);
  }
  
  // Add timestamp fact
  facts.push(`sensor_reading('${sensor}', ${Date.now()})`);
  
  return facts;
}

/**
 * Process telemetry data
 */
export async function processTelemetry(req: Request, res: Response): Promise<void> {
  try {
    const { telemetry }: TelemetryInput = req.body;
    
    // Ensure MCP client is connected
    if (!mcpPrologClient.isConnected()) {
      await mcpPrologClient.connect();
    }

    // Create telemetry session if not exists
    if (!telemetrySessionId) {
      telemetrySessionId = `telemetry_session_${Date.now()}`;
      await mcpPrologClient.createSession(telemetrySessionId, 'iot-telemetry');
    }

    // Convert telemetry to Prolog facts and assert them
    const facts = telemetryToPrologFacts(telemetry);
    const alerts: Record<string, unknown>[] = [];
    
    for (const fact of facts) {
      await mcpPrologClient.assertFact(telemetrySessionId, fact);
    }

    // Apply rules to check for alerts
    try {
      const alertQuery = await mcpPrologClient.query(
        telemetrySessionId, 
        `alert(${telemetry.sensor}, Alert)`
      );
      
      if (alertQuery.success && alertQuery.payload.length > 0) {
        alerts.push(...alertQuery.payload);
      }
    } catch (queryError) {
      // Alert query is optional, don't fail the whole operation
      logger.debug('No alert rules matched', { sensor: telemetry.sensor });
    }

    const result: TelemetryResult = {
      status: 'processed',
      alerts,
    };

    res.json(result);
  } catch (error: any) {
    logger.error('Error processing telemetry', { error: error.message });
    res.status(500).json({ error: 'Error processing telemetry' });
  }
}

/**
 * Get telemetry status
 */
export async function getTelemetryStatus(req: Request, res: Response): Promise<void> {
  try {
    // Return mock status if no session
    if (!telemetrySessionId || !mcpPrologClient.isConnected()) {
      const defaultStatus: TelemetryStatus[] = [
        { sensor: 'light1', value: 'on' },
      ];
      res.json(defaultStatus);
      return;
    }

    // Query all sensor values from Prolog
    const result = await mcpPrologClient.query(
      telemetrySessionId,
      'sensor_value(Sensor, Value)'
    );

    if (result.success && result.payload.length > 0) {
      const status: TelemetryStatus[] = result.payload.map((binding: any) => ({
        sensor: binding.Sensor || 'unknown',
        value: binding.Value || 0,
      }));
      res.json(status);
    } else {
      res.json([{ sensor: 'light1', value: 'on' }]);
    }
  } catch (error: any) {
    logger.error('Error getting telemetry status', { error: error.message });
    res.status(500).json({ error: 'Error getting telemetry status' });
  }
}

/**
 * Reset telemetry session
 */
export async function resetTelemetrySession(): Promise<void> {
  if (telemetrySessionId && mcpPrologClient.isConnected()) {
    await mcpPrologClient.destroySession(telemetrySessionId);
    telemetrySessionId = null;
  }
}
