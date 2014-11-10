var SMath = (function () {
    function SMath() {
    }
    SMath.prototype.fib = function (n) {
        if (n < 2) {
            return n;
        }
        return (this.fib(n - 1) + this.fib(n - 2));
    };

    SMath.prototype.fact = function (n) {
        if (n == 0) {
            return 1;
        }
        return n * this.fact(n - 1);
    };
    return SMath;
})();
exports.SMath = SMath;
