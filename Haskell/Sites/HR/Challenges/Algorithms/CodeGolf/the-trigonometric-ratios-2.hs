import Text.Printf
f n=product[1..n]
a x=c$x:k x 3
b x=c$1:k x 2
k x r=[x**n/f n|n<-[r,r+2..r+6]]
c(x:y)=foldl(\acc(op,n)->acc`op`n)x$zip(cycle[(-),(+)])y
r d=printf"%.3f"(d::Double)
m=map
main=getLine>>getContents>>= \s->putStrLn$unlines$m r$concat$m(\i->[a i,b i])$m read$lines s
