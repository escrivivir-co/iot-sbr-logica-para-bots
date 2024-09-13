const mqtt = require('mqtt');

class MqttService {
  constructor() {
    this.client = null;
  }

  connect(brokerUrl) {
    this.client = mqtt.connect(brokerUrl);

    this.client.on('connect', () => {
      console.log('Connected to MQTT broker');
    });

    this.client.on('error', (error) => {
      console.error('MQTT connection error:', error);
    });
  }

  subscribe(topic) {
    if (!this.client) {
      throw new Error('MQTT client not connected');
    }
    this.client.subscribe(topic, (err) => {
      if (err) {
        console.error('Error subscribing to topic:', err);
      } else {
        console.log(`Subscribed to topic: ${topic}`);
      }
    });
  }

  publish(topic, message) {
    if (!this.client) {
      throw new Error('MQTT client not connected');
    }
    this.client.publish(topic, JSON.stringify(message), (err) => {
      if (err) {
        console.error('Error publishing message:', err);
      } else {
        console.log(`Message published to topic: ${topic}`);
      }
    });
  }

  onMessage(callback) {
    if (!this.client) {
      throw new Error('MQTT client not connected');
    }
    this.client.on('message', (topic, message) => {
      callback(topic, JSON.parse(message.toString()));
    });
  }
}

module.exports = new MqttService();