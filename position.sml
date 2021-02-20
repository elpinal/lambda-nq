structure ByteOffset : POSITION = struct
  type position = int

  val initial = 0

  fun next _ i = i + 1
end
