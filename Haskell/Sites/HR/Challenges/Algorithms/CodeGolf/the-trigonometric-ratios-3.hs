import Text.Printf
f n=product[1..n]
s x=k 3 x x
c=k 2 1
k r z x=e$z-q 0+q 2-q 4+q 6 where q n=x**(r+n)/f(r+n)
e d=printf"%.3f"(d::Double)
m=map
p=putStrLn
main=getLine>>getContents>>=mapM_(\i->(p$s i)>>(p$c i)).m read.lines
