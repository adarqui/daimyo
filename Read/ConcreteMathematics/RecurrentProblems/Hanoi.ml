(*

Hanoi: 8 discs
Brahma: 64 discs

Eight discs stacked in decreasing size on one of three pegs
Objective:
    Transfer the entire tower to one of the other pegs:
    Moving one disc at a time
    Never moving a larger disc onto a smaller disc

Tn = minimum number of moves to transfer n disks
T0 = 0
T1 = 1
T2 = 3
T3 = 7
..

Tn = 2T(n-1)+1 for n>0
Tn = 2^n-1

T0 = 0
T1 = 2^n-1
T2 = 2^n-1
T3 = 2^n-1
T4 = 2^n-1

|_  |  |   |  |   |
|__ |  |   |  |   |
|___|  |   |  |   |

|   |  |   |  |   |
|__ |  |   |  |   |
|___|  |_  |  |   |

|   |  |   |  |   |
|   |  |   |  |   |
|___|  |_  |  |__ |

|   |  |   |  |   |
|   |  |   |  |_  |
|___|  |   |  |__ |

|   |  |   |  |   |
|   |  |   |  |_  |
|   |  |___|  |__ |

|   |  |   |  |   |
|   |  |   |  |   |
|_  |  |___|  |__ |

|   |  |   |  |   |
|   |  |__ |  |   |
|_  |  |___|  |   |
      

|   |  |_  |  |   |
|   |  |__ |  |   |
|   |  |___|  |   |

*)

(*
This is pointless: implement the pearl

open Stack

type hanoi = {
    discs : int;
    pegs : (int t) array;
}

let rec createPeg n s =
    match n with
    | 0 -> s
    | _ -> (push n s; createPeg (n-1) s)
    

let newHanoi n =
    let peg1 = createPeg n (Stack.create ()) in
    let h = { discs = n; pegs = Array.init 3 (fun _ -> Stack.create ()) } in
    let _ = h.pegs.(0) <- peg1 in
    h

let runHanoi n =
*)
