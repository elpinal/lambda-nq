signature ENV = sig
  type t

  val initial : t

  structure Val : sig
    exception Unbound of var
    val lookup : t -> var -> Type.t
    val insert : var -> Type.t -> t -> t
  end

  structure Tag : sig
    exception Unbound of covar
    val lookup : t -> covar -> Type.t
    val insert : covar -> Type.t -> t -> t
    val remove : covar -> t -> t
    val clear : t -> t
  end
end

structure Env :> ENV = struct
  structure V = Map (type t = var val compare = String.compare)
  structure T = Map (type t = covar val compare = String.compare)
  type t = Type.t V.t * Type.t T.t

  val initial = (V.empty, T.empty)

  structure Val = struct
    exception Unbound of var

    fun lookup (x, _) v =
      case V.lookup v x of
           SOME ty => ty
         | NONE    => raise Unbound v

    fun insert v ty (x, y) =
      ( V.insert v ty x
      , y
      )
  end

  structure Tag = struct
    exception Unbound of covar

    fun lookup (_, y) v =
      case T.lookup v y of
           SOME ty => ty
         | NONE    => raise Unbound v

    fun insert v ty (x, y) =
      ( x
      , T.insert v ty y
      )

    fun remove v (x, y) = (x, T.delete v y)

    fun clear (x, _) = (x, T.empty)
  end
end
