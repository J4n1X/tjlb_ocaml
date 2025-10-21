open Parcoom

type position = {
    line: int;
    column: int;
} [@@deriving show]

type 'a positioned = {
    value: 'a;
    pos: position;
} [@@deriving show]

(* Global reference to track the original input text for position calculation *)
let original_text = ref ""

(* Calculate line and column from position in original text *)
let position_from_offset (offset: int): position =
    let text = !original_text in
    let rec count_lines line col i =
        if i >= offset || i >= String.length text then {line; column = col}
        else match String.get text i with
            | '\n' -> count_lines (line + 1) 1 (i + 1)
            | _ -> count_lines line (col + 1) (i + 1)
    in
    count_lines 1 1 0

(* Helper to create a positioned parser *)
let positioned (p: 'a parser): 'a positioned parser =
    { run = fun input ->
        let start_pos = input.pos in
        let input', result = p.run input in
        match result with
        | Ok value -> 
            let pos = position_from_offset start_pos in
            input', Ok {value; pos}
        | Error e -> input', Error e
    }

let is_whitespace (c: char): bool = 
        match c with
        | '\r' | '\t' | ' ' | '\x0C' -> true
        | _ -> false

let trim: string parser = 
    parse_while is_whitespace

let trimmed parser = parser <* trim

let with_optional_minus parser =
    let negative = prefix "-" *> parser |> map (fun value -> "-" ^ value) in
    negative <|> parser

let token_from parser to_token =
    positioned (map to_token parser) |> trimmed

let lexeme_token lexeme token =
    token_from (prefix lexeme) (fun _ -> token)

let is_alpha (c: char): bool = match c with 
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
let is_num (c: char): bool = match c with
    | '0' .. '9' -> true
    | _ -> false
let is_alphanum (c: char) = is_alpha c || is_num c

type token = 
       OParen
     | CParen
     | OCurly
     | CCurly
     | OBracket
     | CBracket
     | StatementEnd         (* ; or newline *)
     | Colon
     | Comma
     | Dot
     | Arrow                 (* -> *)
     | Question             (* ? *)
     (* Arithmetic operators *)
     | Plus                 (* + *)
     | Minus                (* - *)
     | Asterisk             (* * *)
     | Slash                (* / *)
     | Percent              (* % *)
     (* Relational operators *)
     | Equal                (* == *)
     | NotEqual             (* != *)
     | Less                 (* < *)
     | Greater              (* > *)
     | LessEqual            (* <= *)
     | GreaterEqual         (* >= *)
     (* Logical operators *)
     | LogicalAnd           (* && *)
     | LogicalOr            (* || *)
     | LogicalNot           (* ! *)
     (* Bitwise operators *)
     | BitwiseAnd           (* & *)
     | BitwiseOr            (* | *)
     | BitwiseXor           (* ^ *)
     | BitwiseNot           (* ~ *)
     | LeftShift            (* << *)
     | RightShift           (* >> *)
     (* Assignment operators *)
     | Assign               (* = *)
     | PlusAssign           (* += *)
     | MinusAssign          (* -= *)
     | MultAssign           (* *= *)
     | DivAssign            (* /= *)
     | ModAssign            (* %= *)
     | AndAssign            (* &= *)
     | OrAssign             (* |= *)
     | XorAssign            (* ^= *)
     | LeftShiftAssign      (* <<= *)
     | RightShiftAssign     (* >>= *)
     (* Literals *)
     | Integer of int
     | Float of float
     | Ident of string
     | StringLiteral of string
     | Other of string
     [@@ deriving show]

type positioned_token = token positioned [@@deriving show]

let is_single_char_token (s: char) : bool =
    String.contains "(){}[];:,.?~\n" s

let satisfy (pred: char -> bool) (label: string): char parser =
    bind (fun c ->
        if pred c then wrap c else fail ("expected " ^ label)
    ) any_char

let require_result (p: string parser) : string parser =
    bind (fun res -> 
        if String.length res > 0 then wrap res
        else fail "expected non-empty result"
    ) p

let line_comment: string parser =
    prefix "#" *> parse_while (fun c -> c <> '\n' && c <> '\r') <* trim

let block_comment: string parser = 
    prefix "#-" *> parse_while (fun c -> c <> '-') <* prefix "-#" <* trim

let identifier: positioned_token parser =
    let valid_starting_char c = is_alpha c || c = '_' in
    let valid_char c = is_alphanum c || c = '_' in
    let first_letter = map (fun c -> String.make 1 c) (satisfy valid_starting_char "letter or underscore") in
    let rest = parse_while valid_char in
    let ident = map (fun (first, tail) -> first ^ tail) (first_letter <*> rest) in
    token_from ident (fun s -> Ident s)

let integer: positioned_token parser = 
    let digits = require_result (parse_while is_num) in
    let signed_digits = with_optional_minus digits in
    token_from signed_digits (fun text -> Integer (int_of_string text))

let floating: positioned_token parser = 
    let integer_part = require_result (parse_while is_num) in
    let fractional_part = parse_while is_num in
    let base = map (fun ((whole, dot), frac) -> whole ^ dot ^ frac)
        (integer_part <*> prefix "." <*> fractional_part) in
    let signed_value = with_optional_minus base in
    token_from signed_value (fun text -> Float (float_of_string text))

let number = floating <|> integer

let single_char_token: positioned_token parser =
    let to_token (s: char) : token = 
        match s with
        | '(' -> OParen
        | ')' -> CParen
        | '{' -> OCurly
        | '}' -> CCurly
        | '[' -> OBracket
        | ']' -> CBracket
        | ';' | '\n' -> StatementEnd
        | ':' -> Colon
        | ',' -> Comma
        | '.' -> Dot
        | '?' -> Question
        | '~' -> BitwiseNot
        | _ -> failwith "unreachable"
    in
    token_from (satisfy is_single_char_token "single-char token") to_token

(* Multi-character operators - order matters! *)
let multi_char_operator: positioned_token parser =
    let operator_lexemes = [
        ("<<=", LeftShiftAssign);
        (">>=", RightShiftAssign);
        ("->", Arrow);
        ("==", Equal);
        ("!=", NotEqual);
        ("<=", LessEqual);
        (">=", GreaterEqual);
        ("&&", LogicalAnd);
        ("||", LogicalOr);
        ("<<", LeftShift);
        (">>", RightShift);
        ("+=", PlusAssign);
        ("-=", MinusAssign);
        ("*=", MultAssign);
        ("/=", DivAssign);
        ("%=", ModAssign);
        ("&=", AndAssign);
        ("|=", OrAssign);
        ("^=", XorAssign);
        ("+", Plus);
        ("-", Minus);
        ("*", Asterisk);
        ("/", Slash);
        ("%", Percent);
        ("<", Less);
        (">", Greater);
        ("!", LogicalNot);
        ("&", BitwiseAnd);
        ("|", BitwiseOr);
        ("^", BitwiseXor);
        ("=", Assign);
    ] in
    let parser_chain =
        List.fold_right
            (fun (lexeme, token) acc -> lexeme_token lexeme token <|> acc)
            operator_lexemes
            (fail "expected operator")
    in
    parser_chain

let string_literal: positioned_token parser =
    let content_char = satisfy (fun c -> c <> '"' && c <> '\\') "string content" in
    let escape_sequence = 
        prefix "\\" *>
        (satisfy (fun c -> List.mem c ['\\'; '"'; 'n'; 'r'; 't']) "escape character" |> map (fun c -> match c with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | '"' -> '"'
            | '\\' -> '\\'
            | _ -> failwith "unreachable"
        ))
    in
    let string_content = many (content_char <|> escape_sequence) |> map (fun chars -> String.concat "" (List.map (String.make 1) chars)) in
    let quoted = prefix "\"" *> string_content <* prefix "\"" in
    token_from quoted (fun s -> StringLiteral s)

let operator: positioned_token parser = multi_char_operator

let comment_or_token: positioned_token option parser =
    (line_comment *> wrap None) <|>
    (block_comment *> wrap None) <|>
    (map (fun t -> Some t) (number <|> operator <|> string_literal <|> identifier <|> single_char_token)) <* trim

(* Main parser that also sets up position tracking *)
let parser_with_positions (input_text: string): positioned_token list parser = 
    original_text := input_text;
    trim *> many comment_or_token |> map (List.filter_map (fun x -> x))

let parser: positioned_token list parser = 
    trim *> many comment_or_token |> map (List.filter_map (fun x -> x))


(* Code for the actual code blocks begin here *)
type keyword =
      Fn
    | Extern
    | Const
    | Type
    | Struct
    | While
    | If
    | Else
    | Elif
    | For
    | Switch
    | Break
    | Continue
    [@@ deriving show]

let ident_to_keyword (s: string): (keyword, string) result = 
    match s with
    | "fn" -> Ok Fn
    | "extern" -> Ok Extern
    | "const" -> Ok Const
    | "type" -> Ok Type
    | "struct" -> Ok Struct
    | "while" -> Ok While
    | "if" -> Ok If
    | "else" -> Ok Else
    | "elif" -> Ok Elif
    | "for" -> Ok For
    | "switch" -> Ok Switch
    | "break" -> Ok Break
    | "continue" -> Ok Continue
    | _ -> Error ("unknown keyword: " ^ s)

(* Predefined primitive types. These are treated as identifiers in the lexer but recognized as types by the parser. void is an alias for u0 and tokenized as an identifier.
u8 (unsigned 8-bit integer)
i8 (signed 8-bit integer)
u16 (unsigned 16-bit integer)
i16 (signed 16-bit integer)
u32 (unsigned 32-bit integer)
i32 (signed 32-bit integer)
u64 (unsigned 64-bit integer)
i64 (signed 64-bit integer)
f32 (32-bit floating point)
f64 (64-bit floating point)
u0 (void type, equivalent to void)
*)

type lang_type =
      SignedInt of int  (* i8, i16, i32, i64 *)
    | UnsignedInt of int  (* u8, u16, u32, u64 *)
    | Float of int  (* f32, f64 *)
    | Void  (* u0 or void *)
    | CustomType of string  (* user-defined types *)
    [@@ deriving show]

(* For now, we won't support custom types, and this is the most basic way to implement this parsing *)
let ident_to_type (s: string): (lang_type, string) result = 
    match s with
    | "i8" -> Ok (SignedInt 8)
    | "i16" -> Ok (SignedInt 16)
    | "i32" -> Ok (SignedInt 32)
    | "i64" -> Ok (SignedInt 64)
    | "u8" -> Ok (UnsignedInt 8)
    | "u16" -> Ok (UnsignedInt 16)
    | "u32" -> Ok (UnsignedInt 32)
    | "u64" -> Ok (UnsignedInt 64)
    | "f32" -> Ok (Float 32)
    | "f64" -> Ok (Float 64)
    | "u0" | "void" -> Ok Void
    | _ -> Error ("unknown type: " ^ s)

