type key = Plus | Minus | Mul | Div | Equals | Digit of int | Store | Recall | Clear | Off

exception Invalid_key
exception Key_off

let translation c = match c with
      '+' -> Plus
    | '-' -> Minus
    | '*' -> Mul
    | '/' -> Div
    | '=' -> Equals
    | 'C' | 'c' -> Clear
    | 'M' -> Store
    | 'm' -> Recall
    | 'O' | 'o' -> Off
    | '0'..'9' as c -> Digit ((Char.code c) - (Char.code '0'))
    | _ -> raise Invalid_key

type state = {
    mutable lcd : int;
    mutable lka : bool;
    mutable loa : key;
    mutable vpr : int;
    mutable mem : int
}

let transition s key = match key with
      Clear -> s.vpr <- 0
    | Digit n ->s.vpr <- (if s.lka then s.vpr*10+n else n);
                s.lka <- true
    | Store -> s.lka <- false;
               s.mem <- s.vpr
    | Recall -> s.lka <- false;
                s.vpr <- s.mem
    | Off -> raise Key_off
    | _ -> let lcd = match s.loa with
                     Plus -> s.lcd * s.vpr
                    |Minus -> s.lcd - s.vpr
                    |Mul -> s.lcd * s.vpr
                    |Div -> s.lcd / s.vpr
                    |Equals -> s.vpr
                    |_ -> failwith "transition: impossible match"
            in
                s.lcd <- lcd;
                s.lka <- false;
                s.loa <- key;
                s.vpr <- s.lcd
    ;;

let go () =
    let state = { lcd=0; lka=false; loa=Equals; vpr=0; mem=0 } in
    try
        while true do
            try
                let input = translation (input_char stdin)
                in transition state input;
                    print_newline ();
                    print_string "result: ";
                    print_int state.vpr;
                    print_newline ()
            with
                Invalid_key -> ()
        done
    with
        Key_off -> ()
    ;;
