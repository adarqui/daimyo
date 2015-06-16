import time
def follow(file):
    file.seek(0,2)
    while True:
        line = file.readline()
        if not line:
            time.sleep(0.1)
            continue
        yield line

def grep(pattern, lines):
    for line in lines:
        if pattern in line:
            yield line

def watchfile(file):
    f = open(file)
    for line in follow(f):
        print line,

def t1(file):
    lines = grep("python", watchfile(file))
    for line in lines:
        print line,
