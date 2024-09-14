const request = require('supertest');
const app = require('../src/app');
const sqlite3 = require('sqlite3').verbose();

describe('Rule API Integration Tests', () => {
  let db;

  beforeAll(() => {
    db = new sqlite3.Database('./database-test.sqlite');
  });

  afterAll((done) => {
    db.close(() => {
      done();
    });
  });

  it('should create a new rule and retrieve it', async () => {
    const newRule = { text: 'test_rule(X) :- X > 0.' };
    
    // Create a new rule
    const createResponse = await request(app)
      .post('/api/rules')
      .send(newRule)
      .expect(201);

    expect(createResponse.body).toHaveProperty('id');
    expect(createResponse.body.text).toBe(newRule.text);

    // Get all rules
    const getResponse = await request(app)
      .get('/api/rules')
      .expect(200);

    expect(Array.isArray(getResponse.body)).toBeTruthy();
    expect(getResponse.body.some(rule => rule.text === newRule.text)).toBeTruthy();
  });
});
