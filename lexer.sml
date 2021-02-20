structure LexerError = struct
  exception IllegalChar of ByteOffset.position * char
end

structure Lexer : sig
  val lex : string -> Token.t Stream.stream
end = struct
  structure R = Reader (ByteOffset)

  open LexerError

  fun ident r : string =
  let
    val cs = [R.next r]
    fun go cs =
      let val c = R.peek r handle R.EOF => #" " in
        if Char.isAlphaNum c orelse c = #"_"
        then go (c :: cs before R.proceed r c)
        else String.implode (rev cs)
      end
  in
    go cs
  end

  fun lower r =
    case ident r of
         "fn"     => Token.FN
       | "fst"    => Token.FST
       | "snd"    => Token.SND
       | "inl"    => Token.INL
       | "inr"    => Token.INR
       | "case"   => Token.CASE
       | "end"    => Token.END
       | "throw"  => Token.THROW
       | "catch"  => Token.CATCH
       | "bottom" => Token.BOTTOM
       | "unit"   => Token.UNIT
       | s        => Token.LOWER_IDENT s

  val upper = Token.UPPER_IDENT o ident

  (* Assume '-' is already consumed. *)
  fun hyphen r =
    case R.peek_option r of
         SOME #">" => (R.proceed r #">"; Token.RARROW)
       | _         => Token.MINUS

  (* Assume '=' is already consumed. *)
  fun equal r =
    case R.peek_option r of
         SOME #">" => (R.proceed r #">"; Token.RDARROW)
       | _         => Token.EQUAL

  fun lex1 r =
  let
    val start = R.pos r
    val c = R.peek r
  in
    case c of
         #" "  => (R.proceed r c; lex1 r)
       | #"\n" => (R.proceed r c; lex1 r)
       | #"\t" => (R.proceed r c; lex1 r)
       | #"\r" => (R.proceed r c; lex1 r)

       | #"(" => (R.proceed r c; Token.LPAREN)
       | #")" => (R.proceed r c; Token.RPAREN)

       | #":" => (R.proceed r c; Token.COLON)
       | #"*" => (R.proceed r c; Token.STAR)
       | #"," => (R.proceed r c; Token.COMMA)
       | #"+" => (R.proceed r c; Token.PLUS)
       | #"|" => (R.proceed r c; Token.BAR)
       | #"-" => (R.proceed r c; hyphen r)
       | #"=" => (R.proceed r c; equal r)
       | _    =>
           if Char.isLower c
           then lower r
           else if Char.isUpper c
           then upper r
           else raise IllegalChar(start, c)
  end

  fun lex s =
  let
    val r = R.new s
    fun go acc = go (lex1 r :: acc)
      handle R.EOF => acc
  in
    Stream.fromList (rev (go []))
  end
end
