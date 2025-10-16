open Parser

let show_result (results: positioned_token list) = 
    List.iter (fun pt -> Format.printf "%a at line %d, column %d\n" pp_token pt.value pt.pos.line pt.pos.column) results

let show_positioned_result (results: positioned_token list) = 
    List.iter (fun pt -> Format.printf "%a\n" pp_positioned_token pt) results

let test_basic () = 
    Printf.printf "=== Testing Basic Tokens ===\n";
    match ("  test123(   ) { } #- block comment here -# [ ] ; : 21 -69\n# Line comment here\n identifier" |> Parcoom.run parser) with
    | Ok result -> show_result result
    | Error {desc; pos} -> Printf.printf "Error at pos %d: %s\n" pos desc

let test_numbers () = 
    Printf.printf "\n=== Testing Numbers (Integers and Floats) ===\n";
    let number_string = "42 -17 3.14 -2.5 0.0 123.456" in
    match (number_string |> Parcoom.run parser) with
    | Ok result -> show_result result
    | Error {desc; pos} -> Printf.printf "Error at pos %d: %s\n" pos desc

let test_keywords () = 
    Printf.printf "\n=== Testing Keywords ===\n";
    let keyword_string = "fn extern const type struct while if else elif for switch break continue" in
    match (keyword_string |> Parcoom.run parser) with
    | Ok result -> 
        Printf.printf "\nTesting keyword conversion:\n";
        List.iter (fun positioned_token ->
            match positioned_token.value with
            | Ident s -> (
                match ident_to_keyword s with
                | Ok kw -> Printf.printf "%s -> %s\n" s (show_keyword kw)
                | Error err -> Printf.printf "%s -> %s\n" s err
            )
            | _ -> ()
        ) result
    | Error {desc; pos} -> Printf.printf "Error at pos %d: %s\n" pos desc

let read_and_parse_file (filename: string) =
    let ic = open_in filename in
    let rec read_lines acc =
        try
            let line = input_line ic in
            read_lines (acc ^ line ^ "\n")
        with End_of_file -> acc
    in
    let content = read_lines "" in
    close_in ic;
    match Parcoom.run (parser_with_positions content) content with
    | Ok result -> 
        Printf.printf "Parsed tokens from file %s:\n" filename;
        show_result result
    | Error {desc; pos} -> Printf.printf "Error at pos %d: %s\n" pos desc

let () = 
    (*test_basic ();
    test_numbers ();
    test_keywords ()*)
    let filename = "test_input.txt" in
    read_and_parse_file filename