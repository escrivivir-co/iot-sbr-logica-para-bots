/**
 * Winston Logger Configuration
 * 
 * Provides structured logging for the PrologEditor backend.
 */

import winston from 'winston';

console.log('[DEBUG] logger.ts: module evaluating');

const { combine, timestamp, printf, colorize } = winston.format;

const logFormat = printf(({ level, message, timestamp: ts, ...meta }) => {
  const metaStr = Object.keys(meta).length ? ` ${JSON.stringify(meta)}` : '';
  return `${ts} [${level}]: ${message}${metaStr}`;
});

const createLogger = () => {
  try {
    return winston.createLogger({
      level: process.env.LOG_LEVEL || 'info',
      format: combine(
        timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
        logFormat
      ),
      transports: [
        new winston.transports.Console({
          format: combine(colorize(), logFormat),
        }),
        new winston.transports.File({
          filename: 'error.log',
          level: 'error',
        }),
        new winston.transports.File({
          filename: 'combined.log',
        }),
      ],
    });
  } catch (error) {
    // Fallback to console if winston fails
    return {
      info: console.log,
      error: console.error,
      warn: console.warn,
      debug: console.log,
    } as unknown as winston.Logger;
  }
};

export const logger = createLogger();
console.log('[DEBUG] Logger initialized:', !!logger);

export default logger;
