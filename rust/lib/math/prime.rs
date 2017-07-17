use num::integer;



pub fn is_relatively_prime(a: u64, b: u64) -> bool {
  integer::gcd(a, b) == 1
}


// pub fn euler_phi(a: u64, b: u64) ->
