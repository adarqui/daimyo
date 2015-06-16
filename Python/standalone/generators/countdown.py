def countdown(n):
    while n > 0:
        yield n
        n -= 1

def c1():
    for i in countdown(5):
        print i

def c2():
    x = countdown(10)
    print x.next()
    print x.next()
