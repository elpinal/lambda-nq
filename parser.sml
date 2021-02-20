structure ParserError = struct
  exception UnexpectedEOF
  exception UnexpectedToken of Token.t
end

structure Parser = MakeParser (
  structure Streamable = StreamStreamable

  structure Arg = struct
    open Term
    open ParserError

    datatype terminal = datatype Token.t

    type string = string
    type ty = Type.t
    type term = Term.t

    fun term_id x = x
    val tabs = Abs
    val tapp = App
    val tcase = Case
    val tcatch = Catch
    fun tfst x = Proj(Fst, x)
    fun tsnd x = Proj(Snd, x)
    fun tinl (x, ty) = Inj(Fst, x, ty)
    fun tinr (x, ty) = Inj(Snd, x, ty)
    val tpair = Pair
    val tthrow = Throw
    val tvar = Var
    fun tunit () = Unit

    local open Type in
      fun type_id x = x
      val tyarrow = Arrow
      val typrod = Prod
      val tysum = Sum
      fun tybottom () = Bottom
      fun tyunit () = Unit
    end

    fun error (s : Token.t Stream.stream) =
    let open Stream in
      case front s of
           Nil        => UnexpectedEOF
         | Cons(t, _) => UnexpectedToken t
    end
  end
)
