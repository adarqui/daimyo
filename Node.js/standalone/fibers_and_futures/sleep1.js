var Fiber = require('fibers');

function sleep(ms) {
    var fiber = Fiber.current;
    setTimeout(function() {
        fiber.run();
    }, ms);
    Fiber.yield();
}

var sync = Fiber(function() {
    console.log('1... ' + new Date);
    sleep(2000);
    console.log('2... ' + new Date);
    sleep(2000);
    console.log('go!! ' + new Date);
});

console.log('on 3 we go!');
sync.run();
console.log('yay!!');
