(* Part 1 *)

fun qsort nil f = nil
  | qsort [x] f = [x]
  | qsort (pivot::l) f =
    let
        fun partition lt gt nil = (lt, gt)
          | partition lt gt (x::l) =
            if f x pivot
            then partition (x::lt) gt l
            else partition lt (x::gt) l
        val (lt, gt) = partition nil nil l
    in
        qsort lt f @ [pivot] @ qsort gt f
    end


(* Part 2 *)

fun qsHigh nil f = nil
  | qsHigh [x] f = [x]
  | qsHigh (pivot::l) f =
    let
        val (lt, gt) = foldr
            (fn (x, (lt, gt)) =>
                if f x pivot
                then (x::lt, gt)
                else (lt, x::gt))
            (nil, nil) l
    in
        qsHigh lt f @ [pivot] @ qsHigh gt f
    end


(* Part 3 *)

fun cross nil b = nil
  | cross (x::a) b =
    let
        fun subcross nil = nil
          | subcross (y::b) = (x, y) :: subcross b
    in
        subcross b @ cross a b
    end


(* Part 4 *)

fun crossHigh a b =
    foldr op@ nil
        (map (fn x => map (fn y => (x, y)) b) a)


(* Part 5 *)

fun crossFold a b =
    foldr (fn (x, u) =>
        foldr (fn (y, v) =>
            (x, y)::v) u b) nil a
