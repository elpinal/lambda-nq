type var = string
type covar = string

structure Term = struct
  type ty = Type.t

  datatype index
    = Fst
    | Snd

  fun index Fst (x, _) = x
    | index Snd (_, y) = y

  datatype t
    = Var of var
    | Abs of var * ty * t
    | App of t * t
    | Pair of t * t
    | Proj of index * t
    | Inj of index * t * ty
    | Case of t * var * t * var * t
    | Throw of covar * t * ty
    | Catch of covar * ty * t
    | Unit
end
