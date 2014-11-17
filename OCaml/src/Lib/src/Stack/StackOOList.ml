module StackOOList =
    struct
        class ['a] stack =
            object (self)
                val mutable list = ([] : 'a list)
                val mutable sz = (0 : int)
                method push x =
                    list <- x :: list;
                    sz <- sz + 1
                method pop =
                    match list with
                    | [] -> None
                    |  _ ->
                            let result = List.hd list in
                            list <- List.tl list;
                            sz <- sz - 1;
                            Some result
                method peek =
                    match list with
                    | [] -> None
                    | (h::t) -> Some h
                method size =
                    sz
                method fromList l =
                    let _ = List.map (fun a -> self#push a) l in self
                method toList =
                    List.map (fun _ -> let (Some e) = self#pop in e) list
            end
    end

let test l =
    let stk = new StackOOList.stack in
    stk#fromList l
