structure Statics = struct
  open Term

  infix |>
  fun x |> f = f x

  exception NotArrow of Type.t
  exception NotProd of Type.t
  exception NotSum of Type.t

  fun type_of env =
    fn Var v         => Env.Val.lookup env v
     | Abs(v, ty, x) => Type.Arrow(ty, type_of (env |> Env.Val.insert v ty |> Env.Tag.clear) x)
     | App(x, y)     =>
         let
           val (ty11, ty12) = type_of env x |> Type.get_arrow NotArrow
           val ty2 = type_of env y
         in
           ty12 before Type.equal ty11 ty2
         end
    | Pair(x, y)    => Type.Prod (type_of env x, type_of env y)
    | Proj(i, x)    => type_of env x |> Type.get_prod NotProd |> index i
    | Inj(i, x, ty) =>
        let val p = Type.get_sum NotSum ty in
          ty before Type.equal (type_of env x) (index i p)
        end
    | Case(x, v, y, w, z) =>
        let
          val (ty1, ty2) = type_of env x |> Type.get_sum NotSum
          val ty1 = type_of (env |> Env.Val.insert v ty1) y
          val ty2 = type_of (env |> Env.Val.insert w ty2) z
        in
          ty1 before Type.equal ty1 ty2
        end
    | Throw(u, x, ty) =>
        let
          val ty1 = Env.Tag.lookup env u
          val env = env |> Env.Tag.remove u
          val ty2 = type_of env x
        in
          ty before Type.equal ty1 ty2 (* Weakening *)
        end
    | Catch(u, ty, x) =>
        let
          val env = env |> Env.Tag.insert u ty
        in
          case type_of env x of
               Type.Bottom => ty (* EFQ *)
             | ty1         => ty before Type.equal ty1 ty (* Contraction *)
        end
    | Unit => Type.Unit
end
