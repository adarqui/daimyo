open List

type key = Plus | Minus | Mul | Div | Equals | Digit of int

type state = {
    lcd : int;
    lka : key;
    loa : key;
    vpr : int
}

let is_digit = function x -> (x>=0) && (x<=9)

let valid ky = match ky with
      Digit n -> is_digit n
    | _ -> true

let evaluate x y ky = match ky with
      Plus -> x + y
    | Minus -> x - y
    | Mul -> x * y
    | Div -> x / y
    | Equals -> y
    | Digit _ -> failwith "evaluate : no op"

let transition st ky =
    let digit_transition n = function
          Digit _ -> { st with lka=ky; vpr=st.vpr*10+n }
        | _ -> { st with lka=ky; vpr=n }
    in
        match ky with
              Digit p -> digit_transition p st.lka
            | _ -> let res = evaluate st.lcd st.vpr st.loa
                    in { lcd=res; lka=ky; loa=ky; vpr=res }

let transition_list st ls = List.fold_left transition st ls

let initial_state = { lcd=0; lka=Equals; loa=Equals; vpr=0 }
let state2 = transition initial_state (Digit 3)
let state3 = transition state2 Plus
let state4 = transition state3 (Digit 2)
let state5 = transition state4 (Digit 1)
let state6 = transition state5 Mul
let state7 = transition state6 (Digit 2)
let state8 = transition state7 Equals
