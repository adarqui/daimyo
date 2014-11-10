export interface Math {
 fib(n : number) : number;
 fact(n : number) : number;
 times(n : number, m : number) : number;
 double(n : number) : number;
 induction(base : number, comb : (x : number, y : number) => number, n : number) : number;
 induction_fact(n : number) : number;
 induction_sum_int(n : number) : number;
 induction_sum_sqr(n : number) : number;
}


export class SMath implements Math {

 fib(n : number) {
  if(n < 2) {
   return n;
  }
  return (this.fib(n - 1) + this.fib(n - 2));
 }

 fact(n : number) : number{
  if(n == 0) {
   return 1;
  }
  return n * this.fact(n - 1);
 }

 times(n : number, m : number) : number {
  return n * m;
 }

 double(n : number) : number {
  return this.times(n, 2);
 }

 induction(base : number, comb : (x : number, y : number ) => number, n : number) : number {
  if (n == 0) {
   return base;
  } else {
   comb(n, this.induction(base, comb, n-1));
  } 
 }

 induction_fact(n : number) : number {
  return this.induction(1, (x, y) => { return x * y; }, n);
 }

 induction_sum_int(n : number) : number {
  return this.induction(0, (x, y) => { return x + y; }, n);
 }

 induction_sum_sqr(n : number) : number {
  return this.induction(0, (x, y) => { return x * x + y; }, n);
 }

}
