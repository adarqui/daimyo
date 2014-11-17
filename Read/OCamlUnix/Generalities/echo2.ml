let echo () =
    let argv' = Array.to_list Sys.argv in
    if (List.length argv') > 1 then
        let argv = String.concat " " (List.tl argv' @ ["\n"]) in
        print_string argv
    ;;

echo ()
