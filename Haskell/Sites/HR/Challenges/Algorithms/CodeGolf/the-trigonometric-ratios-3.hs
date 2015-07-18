import Text.Printf
f n=product[1..n]
s x=k 3 x x
c=k 2 1
k r z x=z-q 0+q 2-q 4+q 6 where q n=x**(r+n)/f(r+n)
r d=printf"%.3f"(d::Double)
m=map
p=putStrLn
main=getLine>>getContents>>= \l->mapM_(\i->(p$r$s i)>>(p$r$c i))$m read$lines l
