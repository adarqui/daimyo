vows = require('vows')
assert = require('assert')
math = require('../lib/math.js')

vows
 .describe('Fib')
 .addBatch
  'fib 0': 
   topic: () ->
    math.fib 0
   'should return 0': (topic) -> assert.equal topic, 0
  'fib 1':
   topic: () -> math.fib 1
   'should return 1': (topic) -> assert.equal topic, 1 
  'fib 20':
   topic: () -> math.fib 20
   'should return 6765': (topic) -> assert.equal topic, 6765
 .export(module)

vows
 .describe('Fact')
 .addBatch
  'fact 0':
   topic: () -> math.fact 0
   'should return 1': (topic) -> assert.equal topic, 1
  'fact 5':
   topic: () -> math.fact 5
   'should return 120': (topic) -> assert.equal topic, 120
 .export(module)


vows
 .describe('Times')
 .addBatch
  'times 0 5':
   topic: () -> math.times 0, 5
   'should return 0': (topic) -> assert.equal topic, 0
  'times 5 10':
   topic: () -> math.times 5, 10
   'should return 50': (topic) -> assert.equal topic, 50
 .export(module)


vows
 .describe('Double')
 .addBatch
  'double 5':
   topic: () -> math.double 5
   'should return 10': (topic) -> assert.equal topic, 10
 .export(module)
