(* This won't use parcoom, instead it'll tokenize and parse on it's own.*)

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
     | KeywordToken of keyword
     | StringLiteral of string
     | Other of string
     [@@ deriving show]

type position = {
    line: int;
    column: int;
} [@@deriving show]

type 'a positioned = {
    value: 'a;
    pos: position;
} [@@deriving show]

type positioned_token = token positioned [@@deriving show]


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

let is_alpha (c: char): bool = match c with 
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
let is_num (c: char): bool = match c with
    | '0' .. '9' -> true
    | _ -> false
let is_alphanum (c: char) = is_alpha c || is_num c

let is_whitespace (c: char): bool = 
        match c with
        | '\r' | '\t' | ' ' | '\x0C' -> true
        | _ -> false

type scanner = {
  input: string;
  mutable last_token: positioned_token option;
  mutable index: int;
  mutable line: int;
  mutable column: int;
}

let make_scanner (s: string): scanner = {
    input = s;
    last_token = None;
    index = 0;
    line = 1;
    column = 1;
}

exception SyntaxError of string * position 
let raise_syntax_error (msg: string) (sc: scanner): 'a =
    let pos = { line = sc.line; column = sc.column } in
    raise (SyntaxError (msg, pos))

let eof (sc: scanner): bool =
    sc.index >= String.length sc.input
let peek_char (sc: scanner): char option =
    if eof sc then None
    else Some (String.get sc.input sc.index)
let consume_char (sc: scanner): char =
    if eof sc then raise_syntax_error "Unexpected end of input" sc
    else
        let c = String.get sc.input sc.index in
        sc.index <- sc.index + 1;
        if c = '\n' then (
            sc.line <- sc.line + 1;
            sc.column <- 1;
        ) else (
            sc.column <- sc.column + 1;
        );
        c
let rec skip_whitespace (sc: scanner): unit =
    match peek_char sc with
    | Some c when is_whitespace c ->
        let _ = consume_char sc in
        skip_whitespace sc
    | _ -> ()

let rec scan_identifier (sc: scanner): (token) =
  let start = sc.index in
  let ident = match peek_char sc with
    | Some c when is_alpha c ->
      let _ = consume_char sc in
      let rec consume_more () =
        match peek_char sc with
        | Some c when is_alphanum c || c = '_' ->
          let _ = consume_char sc in
          consume_more ()
        | _ -> ()
      in
      consume_more ();
      String.sub sc.input start (sc.index - start)
    | _ -> raise_syntax_error "Expected identifier" sc
    in
    match ident_to_keyword ident with
    | Ok kw -> KeywordToken kw
    | Error _ -> Ident ident 

let scan_number (sc: scanner): (token) =
  let start = sc.index in
  let rec consume_digits () =
    match peek_char sc with
    | Some c when is_num c ->
      let _ = consume_char sc in
      consume_digits ()
    | _ -> ()
  in
  consume_digits ();
  match peek_char sc with
  | Some '.' ->
    let _ = consume_char sc in
    consume_digits ();
    let num_str = String.sub sc.input start (sc.index - start) in
    Float (float_of_string num_str)
  | _ ->
    let num_str = String.sub sc.input start (sc.index - start) in
    Integer (int_of_string num_str)
let scan_operator (sc: scanner): token =
  match peek_char sc with
  | None -> raise_syntax_error "Unexpected end of input" sc
  | Some '<' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); LessEqual
    | Some '<' ->
      ignore (consume_char sc);
      begin match peek_char sc with
      | Some '=' -> ignore (consume_char sc); LeftShiftAssign
      | _ -> LeftShift
      end
    | _ -> Less
    end
  | Some '>' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '>' ->
      ignore (consume_char sc);
      begin match peek_char sc with
      | Some '=' -> ignore (consume_char sc); RightShiftAssign
      | _ -> RightShift
      end
    | Some '=' -> ignore (consume_char sc); GreaterEqual
    | _ -> Greater
    end
  | Some '-' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '>' -> ignore (consume_char sc); Arrow
    | Some '=' -> ignore (consume_char sc); MinusAssign
    | _ -> Minus
    end
  | Some '+' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); PlusAssign
    | _ -> Plus
    end
  | Some '*' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); MultAssign
    | _ -> Asterisk
    end
  | Some '/' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); DivAssign
    | _ -> Slash
    end
  | Some '%' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); ModAssign
    | _ -> Percent
    end
  | Some '!' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); NotEqual
    | _ -> LogicalNot
    end
  | Some '=' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); Equal
    | _ -> Assign
    end
  | Some '&' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '&' -> ignore (consume_char sc); LogicalAnd
    | Some '=' -> ignore (consume_char sc); AndAssign
    | _ -> BitwiseAnd
    end
  | Some '|' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '|' -> ignore (consume_char sc); LogicalOr
    | Some '=' -> ignore (consume_char sc); OrAssign
    | _ -> BitwiseOr
    end
  | Some '^' ->
    ignore (consume_char sc);
    begin match peek_char sc with
    | Some '=' -> ignore (consume_char sc); XorAssign
    | _ -> BitwiseXor
    end
  | Some '~' ->
    ignore (consume_char sc);
    BitwiseNot
  | Some _ -> raise_syntax_error "Unknown operator" sc
