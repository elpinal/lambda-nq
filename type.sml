structure Type = struct
  datatype t
    = Bottom
    | Unit
    | Sum of t * t
    | Prod of t * t
    | Arrow of t * t

  exception TypeMismatch of t * t

  fun equal x y =
    if x = y
    then ()
    else raise TypeMismatch(x, y)

  fun get_arrow f =
    fn Arrow x => x
     | ty      => raise f ty

  fun get_prod f =
    fn Prod x => x
     | ty     => raise f ty

  fun get_sum f =
    fn Sum x => x
     | ty    => raise f ty
end
