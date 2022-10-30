
type state = {tokens: Token.token array; mutable cur: int}

exception ParseError of string

let advance state =
  state.cur <- state.cur + 1

let peek state =
  state.tokens.(state.cur)

let expect state tok_typ =
  let next = peek state in
  if next.kind = tok_typ then
    advance state
  else let msg = Printf.sprintf "Expected: %s, but get: %s"
  (Token.show_token_type tok_typ) (Token.show_token_type next.kind) in
  raise (ParseError msg)

let rec parse_exp state =
  let next = peek state in
  advance state;
  match next.kind with
  | Token.Identifier ->
      (* expect state Token.Eof; *)
      Ast.Var next.lexeme
  | Token.LeftParen ->
      (let main = peek state in
      match main.kind with
      | Token.Lambda ->
          advance state;
          let var = (peek state).lexeme in
          expect state Token.Identifier;
          let body = parse_exp state in
          expect state Token.RightParen;
          Ast.Abs(var, body)
      | _ ->
          let rator = parse_exp state in
          let after = peek state in
          match after.kind with
          | Token.RightParen -> rator
          | _ -> let rand = parse_exp state in
                 expect state Token.RightParen;
                 Ast.App(rator, rand))
  | _ -> raise (ParseError "error")
