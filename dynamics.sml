structure Dynamics = struct
  open Term

  exception Unbound of var
  exception NotFunction of Term.t
  exception NotPair of Term.t
  exception NotInj of Term.t
  exception ThrowVal of covar * Term.t

  local
    structure E = Map (type t = var val compare = String.compare)
  in
    val initial = E.empty

    fun lookup env v =
      valOf (E.lookup v env)
      handle Option => raise Unbound v

    fun insert v t env =
      E.insert v t env
  end

  fun reduce env =
    fn Var v      => lookup env v
     | t as Abs _ => t
     | App(x, y)  =>
         let
           val v1 = reduce env x
           val v2 = reduce env y
         in
           case v1 of
                Abs(v, _, z) => reduce (insert v v2 env) z
              | _            => raise NotFunction v1
         end
     | Pair(x, y) => Pair(reduce env x, reduce env y)
     | Proj(i, x) =>
         let in
           case reduce env x of
                Pair p => index i p
              | v      => raise NotPair v
         end
     | Inj(i, x, ty) => Inj(i, reduce env x, ty)
     | Case(x, v, y, w, z) =>
         let in
           case reduce env x of
                Inj(Fst, vl, _) => reduce (insert v vl env) y
              | Inj(Snd, vl, _) => reduce (insert w vl env) z
              | v               => raise NotInj v
         end
     | Throw(u, x, _) => raise ThrowVal(u, reduce env x)
     | Catch(u, _, x) =>
         let in
           reduce env x
           handle ThrowVal(u1, v) =>
             if u1 = u
             then v
             else raise ThrowVal(u1, v)
         end
     | Unit => Unit

  val reduce = reduce initial
end
