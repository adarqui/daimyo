/// <reference path="/data/lib/tsUnit/tsUnit.ts" />
/// <reference path="../src/Math.ts" />

import Math = require('../src/Math');

class SimpleTests extends tsUnit.TestClass {
 private target = new Math.SMath();
 fib0() {
  this.areIdentical(this.target.fib(0), 0);
 }
}

var test = new tsUnit.Test();
test.addTestClass(new SimpleTests());
test.showResults(document.getElementById('results'), test.run());
