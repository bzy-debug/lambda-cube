open Token

type scanner = {
  source: string;
  tokens: token list;
  start: int;
  current: int;
  location: location;
}

let make_scanner source =
  {source; tokens = []; start = 0; current = 0; location = {line = 1; spam = (0, 0)}}

let is_at_end scanner = scanner.current >= String.length scanner.source

let peak scanner =
  if is_at_end scanner 
  then '\000'
  else String.get scanner.source scanner.current

let advance scanner = {scanner with current = scanner.current + 1}

let get_lexeme scanner = String.sub scanner.source scanner.start (scanner.current - scanner.start)

let add_token token_type scanner =
  let token = {kind = token_type; lexeme = get_lexeme scanner; location = scanner.location } in
  {scanner with tokens = token :: scanner.tokens}

let next_line no scanner =
  {scanner with location = add_line no scanner.location}

let up_spam scanner =
  {scanner with location = {scanner.location with spam = (scanner.start, scanner.current)}}

let isalpha c = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
let isdigit c = '0' <= c && c <= '9'

let is_valid c =
     isalpha c
  || c = '_' || c = '\''
  || isdigit c

let rec identifier scanner =
  let c = peak scanner in
  if not (is_valid c) then
    let lexeme = get_lexeme scanner in
    match List.assoc_opt lexeme keywords_map with
    | Some t -> add_token t (up_spam scanner)
    | None -> add_token Identifier (up_spam scanner)
  else
    scanner |> advance |> identifier

let scan_token scanner =
  let c = peak scanner in
  let scanner = advance scanner in
  match c with
  | '(' -> add_token LeftParen (up_spam scanner)
  | ')' -> add_token RightParen (up_spam scanner)
  | ' ' | '\t' | '\r' -> up_spam scanner
  | '\n' -> next_line 1 scanner
  | _ -> identifier scanner

let rec scan_tokens scanner =
  if is_at_end scanner then
    let eof = {kind = Eof; lexeme = ""; location = scanner.location} in
    List.rev (eof :: scanner.tokens)
  else
    let scanner = scan_token scanner in
    scan_tokens {scanner with start = scanner.current}
