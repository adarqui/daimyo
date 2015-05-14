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

    SMath.prototype.times = function (n, m) {
        return n * m;
    };

    SMath.prototype.double = function (n) {
        return this.times(n, 2);
    };

    SMath.prototype.induction = function (base, comb, n) {
        if (n == 0) {
            return base;
        } else {
            comb(n, this.induction(base, comb, n - 1));
        }
    };

    SMath.prototype.induction_fact = function (n) {
        return this.induction(1, function (x, y) {
            return x * y;
        }, n);
    };

    SMath.prototype.induction_sum_int = function (n) {
        return this.induction(0, function (x, y) {
            return x + y;
        }, n);
    };

    SMath.prototype.induction_sum_sqr = function (n) {
        return this.induction(0, function (x, y) {
            return x * x + y;
        }, n);
    };
    return SMath;
})();
exports.SMath = SMath;
