var Fiber = require('fibers');

function sleep(ms, cb) {
    setTimeout(function() {
        cb();
    }, ms);
}

console.log('on 3 we go!');

console.log('1... ' + new Date);

sleep(2000, function() {
    console.log('2... ' + new Date);
    sleep(2000, function() {
        console.log('go!! ' + new Date);
    });
});

console.log('yay!!');
