import * as winston from "winston";

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || "info",
  format: winston.format.json(),
  transports:
    process.env.NODE_ENV !== "production"
      ? [
          new winston.transports.Console({
            format: winston.format.simple(),
          }),
        ]
      : [
          new winston.transports.Console({
            format: winston.format.json(),
          }),
        ],
});

export default logger;
