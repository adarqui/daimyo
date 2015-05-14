var
 vows = require('vows'),
 assert = require('assert'),
 math = require('../src/math.js')
 ;

vows.describe('Fib').addBatch({
 'fib 0': {
  topic: function() { return math.fib(0) },
  'should return 0' : function(topic) {
   assert.equal(topic, 0);
  }
 },
 'fib 1': {
  topic: function() { return math.fib(1) },
  'should return 1' : function(topic) {
   assert.equal(topic, 1);
  }
 },
 'fib 20': {
  topic: function() { return math.fib(20) },
  'should return 20' : function(topic) {
   assert.equal(topic, 6765);
  }
 }
}).export(module);


vows.describe('Fact').addBatch({
 'fact 0': {
  topic: function() { return math.fact(0) },
  'should return 1' : function(topic) {
   assert.equal(topic, 1);
  }
 },
 'fact 5': {
  topic: function() { return math.fact(5) },
  'should return 120' : function(topic) {
   assert.equal(topic, 120);
  }
 }
}).export(module);
