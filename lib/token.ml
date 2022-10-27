type token_type =
  | LeftParen
  | RightParen
  | Eof
  | Lambda
  | Identifier
[@@deriving show]

type location = {
  line: int;
  column: int;
}
[@@deriving show]

let add_line no location = {location with line = location.line + no}

let add_column no location = {location with column = location.column + no}

let keywords_map = [
  ("lambda", Lambda);
]

type token = {
  kind: token_type;
  lexeme: string;
  location: location;
}
[@@deriving show]
