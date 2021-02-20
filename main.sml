open LambdaNQ

val args = CommandLine.arguments ()

val filepath =
  case args of
       []     => ",example.txt"
     | x :: _ => x

val t = parse_file filepath
val ty = typecheck t
val t' = eval t
