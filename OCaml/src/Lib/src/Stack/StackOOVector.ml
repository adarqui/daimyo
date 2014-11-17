exception StackSizeOverflow

module StackOOVector =
    struct
        class ['a] stack (max_sz : int) (fill : 'a) =
            object (self)
                val mutable vec = Array.create (max_sz+1) fill
                val mutable sz = (0 : int)
                method push x =
                    vec.(sz) <- x;
                    sz <- sz + 1
                method pop =
                    match sz with
                    | 0 -> None
                    |  _ ->
                            let result = vec.(sz-1) in
                            sz <- sz - 1;
                            Some result
                method peek =
                    match sz with
                    | 0 -> None
                    | _ -> Some (vec.(sz-1))
                method size =
                    sz
                method fromList l =
                    let _ = List.map (fun a -> self#push a) l in self
                method toList =
                    match sz with
                    | 0 -> []
                    | _ -> let (Some v) = self#pop in v :: self#toList
            end
    end

let test fill l =
    let stk = new StackOOVector.stack 100 fill in
    []
