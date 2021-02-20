structure LambdaNQ : sig
  val read_file : string -> string
  val parse_string : string -> Term.t

  val parse_file : string -> Term.t
  val typecheck : Term.t -> Type.t
  val eval : Term.t -> Term.t
end = struct
  fun fail s =
    ( TextIO.output (TextIO.stdErr, s ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun read_file filepath =
  let
    val ins = TextIO.openIn filepath
      handle IO.Io _ => fail ("cannot open: " ^ filepath)
  in
    TextIO.inputAll ins
  end

  fun parse_string s =
  let
    val tokens = Lexer.lex s
    val (t, _) = Parser.parse tokens
  in
    t
  end

  val parse_file = parse_string o read_file

  fun typecheck t = Statics.type_of Env.initial t

  fun eval t = Dynamics.reduce t
end
