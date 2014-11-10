# type mat = { n:int; m:int; t: float array array }; ;
type mat = { n: int; m: int; t: float array array }
# let create mat n m = { n=n; m=m; t = Array.create matrix n m 0.0 } ; ;
val create_mat : int -> int -> mat = <fun>
# let access mat m i j = m.t.(i).(j) ; ;
val access_mat : mat -> int -> int -> float = <fun>
# let mod mat m i j e = m.t.(i).(j) <- e ; ;
val mod_mat : mat -> int -> int -> float -> unit = <fun>
# let a = create mat 3 3 ; ;
val a : mat = {n=3; m=3; t=[|[|0; 0; 0|]; [|0; 0; 0|]; [|0; 0; 0|]|]}
# mod mat a 1 1 2.0; mod mat a 1 2 1.0; mod mat a 2 1 1.0 ; ;
- : unit = ()
# a ; ;
- : mat = {n=3; m=3; t=[|[|0; 0; 0|]; [|0; 2; 1|]; [|0; 1; 0|]|]}

The sum of two matrices a and b is a matrix c such that cij = aij + bij .
# let add mat p q =
if p.n = q.n && p.m = q.m then
let r = create mat p.n p.m in
for i = 0 to p.n-1 do
for j = 0 to p.m-1 do
mod mat r i j (p.t.(i).(j) +. q.t.(i).(j))
done
done ;
r
else failwith "add_mat : dimensions incompatible"; ;
val add_mat : mat -> mat -> mat = <fun>
# add mat a a ; ;
- : mat = {n=3; m=3; t=[|[|0; 0; 0|]; [|0; 4; 2|]; [|0; 2; 0|]|]}
The product of two matrices a and b is a matrix c such that cij =
Pk=ma
k=1 aik.bkj
# let mul mat p q =
if p.m = q.n then
let r = create mat p.n q.m in
for i = 0 to p.n-1 do
for j = 0 to q.m-1 do
let c = ref 0.0 in
for k = 0 to p.m-1 do
c := !c +. (p.t.(i).(k) *. q.t.(k).(j))
done;
mod mat r i j !c
done
done;
r
else failwith "mul_mat : dimensions incompatible" ; ;

# mul mat a a; ;
- : mat = {n=3; m=3; t=[|[|0; 0; 0|]; [|0; 5; 2|]; [|0; 2; 1|]|]}
