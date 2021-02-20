structure Token = struct
  datatype t
    = LOWER_IDENT of string
    | UPPER_IDENT of string

    | LPAREN
    | RPAREN

    | COLON
    | COMMA
    | BAR
    | PLUS
    | MINUS
    | STAR
    | EQUAL
    | RARROW
    | RDARROW

    | FN
    | FST
    | SND
    | INL
    | INR
    | CASE
    | END
    | THROW
    | CATCH
    | BOTTOM
    | UNIT
end
