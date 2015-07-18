import Text.Printf
k r z x=printf"%.3f"((z-q 0+q 2-q 4+q 6)::Double)where q n=x**(r+n)/product[1..r+n]
p=putStrLn
main=getContents>>=mapM_(\i->(p$k 3 i i)>>(p$k 2 1 i)).tail.map read.lines
