datatype 'a Stream = Nil
                   | Cons of 'a * (unit -> 'a Stream)

exception Bad of string

fun from seed next = Cons(seed, fn () => from (next seed) next)

fun head (Nil) = raise Bad("Nil in head")
  | head (Cons(a, b)) = a

fun tail (Nil) = raise Bad("Nil in tail")
  | tail (Cons(a, b)) = b ()

fun take 0 _ = nil
  | take n (Nil) = raise Bad("Nil in take")
  | take n (Cons(a, b)) = a :: (take (n - 1) (b ()))


(* Question 1 *)

val nat = from 1.0 (fn n => n + 1.0)

val one = from 1.0 (fn n => n)

val zero = from 0.0 (fn n => n)

val alt = from 1.0 (fn n => ~n)


(* Question 2 *)

fun mul (a: real Stream) (b: real Stream) =
    Cons((head a) * (head b), fn () => mul (tail a) (tail b))


(* Question 3 *)

fun fs () = Cons(1.0, fn () => mul nat (fs ()))
val fs = fs ()


(* Question 4 *)

fun weave a b = Cons((head a), fn () =>
    Cons((head b), fn () => weave (tail a) (tail b)))


(* Question 5 *)

fun px x = from 1.0 (fn y => y * x)


(* Question 6 *)

fun frac s = Cons(1.0 / (head s), fn () => frac (tail s))


(* Question 7 *)

val coefs = frac fs
fun eval s x n = foldl op+ 0.0 (take n (mul s (px x)))
fun ex x = eval coefs x 10


(* Question 8 *)

fun cosx x = eval (mul (weave alt zero) (frac fs)) x 10
