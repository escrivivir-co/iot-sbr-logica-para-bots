# Prolog Rule Management System

This project is a web application that allows users to manage and run Prolog rules. It consists of an Angular frontend and a NodeJS backend with Express.js, integrated with MQTT and SWI-Prolog.

## Features

- Create, list, and delete Prolog rules
- Execute Prolog rules and view results
- MQTT integration for real-time data processing
- PostgreSQL database for storing rules

## Setup

### Prerequisites

- Node.js (v14 or later)
- Angular CLI
- PostgreSQL
- SWI-Prolog
- MQTT Broker (e.g., Mosquitto)

### Backend Setup

1. Navigate to the `backend` directory
2. Install dependencies: `npm install`
3. Set up environment variables:
   - `DATABASE_URL`: PostgreSQL connection string
   - `PGDATABASE`, `PGHOST`, `PGUSER`, `PGPASSWORD`, `PGPORT`: PostgreSQL connection details
4. Start the server: `node src/app.js`

### Frontend Setup

1. Navigate to the `frontend` directory
2. Install dependencies: `npm install`
3. Start the development server: `ng serve`

## Usage

1. Open a web browser and go to `http://localhost:5001`
2. Use the Rule Editor to create and run Prolog rules
3. View and manage existing rules in the Rule List

## Troubleshooting

- If you encounter MQTT connection issues, check the MQTT broker configuration in `backend/config.js`
- For Invalid Host header issues in WebView, ensure the Angular `serve` command uses the correct host and port settings

## License

This project is licensed under the MIT License.
