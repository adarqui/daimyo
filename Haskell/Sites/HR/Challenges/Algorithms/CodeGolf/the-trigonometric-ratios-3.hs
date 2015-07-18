import Text.Printf
f n=product[1..n]
k r z x=e$z-q 0+q 2-q 4+q 6 where q n=x**(r+n)/f(r+n)
e d=printf"%.3f"(d::Double)
p=putStrLn
main=getContents>>=mapM_(\i->(p$k 3 i i)>>(p$k 2 1 i)).tail.map read.lines
