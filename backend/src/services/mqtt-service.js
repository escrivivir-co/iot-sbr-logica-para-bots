const logger = require('../utils/logger');

let mqttClient;

function init(client) {
  mqttClient = client;
  subscribeToTopics();
}

function subscribeToTopics() {
  const topics = ['sensor/temperature', 'sensor/humidity'];
  topics.forEach(topic => {
    mqttClient.subscribe(topic, (err) => {
      if (err) {
        logger.error(`Error subscribing to ${topic}:`, err);
      } else {
        logger.info(`Subscribed to ${topic}`);
      }
    });
  });

  mqttClient.on('message', (topic, message) => {
    logger.info(`Received message on ${topic}: ${message.toString()}`);
    // Process the message as needed
  });
}

function publishMessage(topic, message) {
  mqttClient.publish(topic, message, (err) => {
    if (err) {
      logger.error(`Error publishing to ${topic}:`, err);
    } else {
      logger.info(`Published to ${topic}: ${message}`);
    }
  });
}

module.exports = {
  init,
  publishMessage
};
