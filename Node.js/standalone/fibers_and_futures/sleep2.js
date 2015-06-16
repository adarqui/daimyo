var Future = require('fibers/future'), wait = Future.wait;

function sleep(ms) {
    var future = new Future;
    setTimeout(function() {
        future.return();
    }, ms);
    return future;
}

var sync = function() {
    console.log('1... ' + new Date);
    sleep(2000).wait();
    console.log('2... ' + new Date);
    sleep(2000).wait();
    console.log('go!! ' + new Date);
}.future();

console.log('on 3 we go!');
sync();
console.log('yay!!');
