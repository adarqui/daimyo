/// <reference path="/data/lib/tsUnit/tsUnit.ts" />
/// <reference path="../src/Math.ts" />
var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var Math = require('../src/Math');

var SimpleTests = (function (_super) {
    __extends(SimpleTests, _super);
    function SimpleTests() {
        _super.apply(this, arguments);
        this.target = new Math.SMath();
    }
    SimpleTests.prototype.fib0 = function () {
        this.areIdentical(this.target.fib(0), 0);
    };
    return SimpleTests;
})(tsUnit.TestClass);

var test = new tsUnit.Test();
test.addTestClass(new SimpleTests());
test.showResults(document.getElementById('results'), test.run());
