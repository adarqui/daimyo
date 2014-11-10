package math

func Fib(n int) int {
 switch n {
  case 0: return 0
  case 1: return 1
  default: return Fib(n-1) + Fib(n-2)
 }
}

func Fact(n int) int {
 switch n {
  case 0: return 1
  default: return n * Fact(n-1)
 }
}


func Times(n, m int) int {
 return n * m;
}


func Double(n int) int {
 return Times(n, 2)
}


func Induction(base int, comb func(x, y int) int, n int) int {
 if n == 0 {
  return base
 } else {
  return comb(n, Induction(base, comb, (n - 1)));
 } 
}

func Induction_Fact(n int) int {
 mul := func(x, y int) int {
  return x * y
 }
 return Induction(1, mul, n)
}

func Induction_Sum_Int(n int) int {
 add := func(x, y int) int {
  return x + y
 } 
 return Induction(0, add, n)
}

func Induction_Sum_Sqr(n int) int {
 sumsqr := func(x, y int) int {
  return x * x + y
 }
 return Induction(0, sumsqr, n)
}
