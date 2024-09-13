exports.telemetryToPrologFacts = (telemetry) => {
  const facts = [];
  for (const [key, value] of Object.entries(telemetry)) {
    facts.push(`telemetry(${key}, ${value})`);
  }
  return facts;
};
