/**
 * Telemetry Routes
 */

import { Router } from 'express';
import * as telemetryController from '../controllers/telemetry.controller';

const router = Router();

router.post('/process', telemetryController.processTelemetry);
router.get('/status', telemetryController.getTelemetryStatus);

export default router;
