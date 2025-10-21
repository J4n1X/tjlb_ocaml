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

let is_ident_start (c: char): bool = is_alpha c || c = '_'
let is_ident_body (c: char): bool = is_alphanum c || c = '_'

let is_operator_start (c: char): bool =
  match c with
  | '<' | '>' | '-' | '+' | '*' | '/' | '%' | '!' | '=' | '&' | '|' | '^' | '~' -> true
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

let rec skip_inline_whitespace (sc: scanner): unit =
  match peek_char sc with
  | Some c when is_whitespace c ->
      ignore (consume_char sc);
      skip_inline_whitespace sc
  | _ -> ()

let current_position (sc: scanner): position = { line = sc.line; column = sc.column }

let emit_token (sc: scanner) (pos: position) (value: token): positioned_token =
  let token = { value; pos } in
  sc.last_token <- Some token;
  token

let peek_char_at (sc: scanner) (offset: int): char option =
  let target = sc.index + offset in
  if target < String.length sc.input then Some (String.get sc.input target) else None

let accept_char (sc: scanner) (expected: char): bool =
  match peek_char sc with
  | Some c when c = expected -> ignore (consume_char sc); true
  | _ -> false

let rec consume_while (sc: scanner) (pred: char -> bool): unit =
  match peek_char sc with
  | Some c when pred c ->
      ignore (consume_char sc);
      consume_while sc pred
  | _ -> ()

let slice_from (sc: scanner) (start: int): string =
  String.sub sc.input start (sc.index - start)

let scan_identifier (sc: scanner): token =
  let start = sc.index in
  ignore (consume_char sc);
  consume_while sc is_ident_body;
  let ident = slice_from sc start in
  match ident_to_keyword ident with
  | Ok kw -> KeywordToken kw
  | Error _ -> Ident ident

let scan_number (sc: scanner): token =
  let start = sc.index in
  consume_while sc is_num;
  let is_float =
    match peek_char sc, peek_char_at sc 1 with
    | Some '.', Some next when is_num next ->
        ignore (consume_char sc);
        consume_while sc is_num;
        true
    | _ -> false
  in
  let lexeme = slice_from sc start in
  if is_float then Float (float_of_string lexeme) else Integer (int_of_string lexeme)

let scan_operator (sc: scanner): token =
  match consume_char sc with
  | '<' ->
      if accept_char sc '<' then
        if accept_char sc '=' then LeftShiftAssign else LeftShift
      else if accept_char sc '=' then LessEqual
      else Less
  | '>' ->
      if accept_char sc '>' then
        if accept_char sc '=' then RightShiftAssign else RightShift
      else if accept_char sc '=' then GreaterEqual
      else Greater
  | '-' ->
      if accept_char sc '>' then Arrow
      else if accept_char sc '=' then MinusAssign
      else Minus
  | '+' ->
      if accept_char sc '=' then PlusAssign else Plus
  | '*' ->
      if accept_char sc '=' then MultAssign else Asterisk
  | '/' ->
      if accept_char sc '=' then DivAssign else Slash
  | '%' ->
      if accept_char sc '=' then ModAssign else Percent
  | '!' ->
      if accept_char sc '=' then NotEqual else LogicalNot
  | '=' ->
      if accept_char sc '=' then Equal else Assign
  | '&' ->
      if accept_char sc '&' then LogicalAnd
      else if accept_char sc '=' then AndAssign
      else BitwiseAnd
  | '|' ->
      if accept_char sc '|' then LogicalOr
      else if accept_char sc '=' then OrAssign
      else BitwiseOr
  | '^' ->
      if accept_char sc '=' then XorAssign else BitwiseXor
  | '~' -> BitwiseNot
  | _ -> raise_syntax_error "Unknown operator" sc

let rec skip_line_comment (sc: scanner): unit =
  match peek_char sc with
  | None | Some '\n' -> ()
  | Some _ ->
      ignore (consume_char sc);
      skip_line_comment sc

let rec skip_block_comment (sc: scanner): unit =
  match peek_char sc with
  | None -> raise_syntax_error "Unterminated block comment" sc
  | Some '-' ->
      ignore (consume_char sc);
      if accept_char sc '#' then ()
      else skip_block_comment sc
  | Some _ ->
      ignore (consume_char sc);
      skip_block_comment sc

let rec skip_line_comment (sc: scanner): unit =
  skip_inline_whitespace sc;
  match peek_char sc with
  | Some '#' ->
      ignore (consume_char sc);
      if accept_char sc '-' then skip_block_comment sc else skip_line_comment sc;
      skip_line_comment sc
  | _ -> ()

let scan_string_literal (sc: scanner): token =
  let buffer = Buffer.create 16 in
  ignore (consume_char sc); (* opening quote *)
  let rec loop () =
    match peek_char sc with
    | None -> raise_syntax_error "Unterminated string literal" sc
    | Some '"' ->
        ignore (consume_char sc);
        StringLiteral (Buffer.contents buffer)
    | Some '\\' ->
        ignore (consume_char sc);
        begin match peek_char sc with
        | None -> raise_syntax_error "Unterminated escape sequence" sc
        | Some esc ->
            let mapped =
              match esc with
              | 'n' -> '\n'
              | 'r' -> '\r'
              | 't' -> '\t'
              | '"' -> '"'
              | '\\' -> '\\'
              | other -> other
            in
            ignore (consume_char sc);
            Buffer.add_char buffer mapped;
            loop ()
        end
    | Some '\n' -> raise_syntax_error "Unterminated string literal" sc
    | Some ch ->
        ignore (consume_char sc);
        Buffer.add_char buffer ch;
        loop ()
  in
  loop ()

let single_char_token (c: char): token option =
  match c with
  | '(' -> Some OParen
  | ')' -> Some CParen
  | '{' -> Some OCurly
  | '}' -> Some CCurly
  | '[' -> Some OBracket
  | ']' -> Some CBracket
  | ';' -> Some StatementEnd
  | ':' -> Some Colon
  | ',' -> Some Comma
  | '.' -> Some Dot
  | '?' -> Some Question
  | _ -> None

let scan_next_token (sc: scanner): positioned_token option =
  let emit value pos = Some (emit_token sc pos value) in
  let rec next () =
    skip_line_comment sc;
    match peek_char sc with
    | None -> sc.last_token <- None; None
    | Some '\n' ->
        let pos = current_position sc in
        ignore (consume_char sc);
        emit StatementEnd pos
    | Some c when c = ';' ->
        let pos = current_position sc in
        ignore (consume_char sc);
        emit StatementEnd pos
    | Some c when is_ident_start c ->
        let pos = current_position sc in
        let value = scan_identifier sc in
        emit value pos
    | Some c when is_num c ->
        let pos = current_position sc in
        let value = scan_number sc in
        emit value pos
    | Some '"' ->
        let pos = current_position sc in
        let value = scan_string_literal sc in
        emit value pos
    | Some c when is_operator_start c ->
        let pos = current_position sc in
        let value = scan_operator sc in
        emit value pos
    | Some c ->
        let pos = current_position sc in
        begin match single_char_token c with
        | Some value ->
            ignore (consume_char sc);
            emit value pos
        | None ->
            let consumed = consume_char sc in
            emit (Other (String.make 1 consumed)) pos
        end
  in
  next ()

let rec scan_all_tokens (sc: scanner) (acc: positioned_token list): positioned_token list =
  match scan_next_token sc with
  | None -> List.rev acc
  | Some token -> scan_all_tokens sc (token :: acc)

let tokenize (input: string): positioned_token list =
  let sc = make_scanner input in
  scan_all_tokens sc []
