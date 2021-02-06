
(* Question 1 *)

fun sumHelper 0 a f = a
  | sumHelper k a f = sumHelper (k - 1) (a + f k) f;

(* 1 *)
fun sum n = sumHelper n 0 (fn k => k);

(* 2 *)
fun sumSq n = sumHelper n 0 (fn k => k * k);

(* 3 *)
fun sumodd n = sumHelper n 0 (fn k => 2 * k - 1);

(* 4 *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 2) + fib (n - 1);

(* 5 *)
fun fibFast n =
    let
        fun helper 0 current next = current
          | helper k current next =
            helper (k - 1) next (current + next)
    in helper n 0 1 end;


(* Question 2 *)

fun sinappx n x =
    let
        fun helper k term sum =
            if k = n then sum
            else 
                let
                    val newk = k + 1
                    val tmp = 2.0 * real newk
                    val newterm = ~term * x * x / tmp / (tmp + 1.0)
                    val newsum = sum + newterm
                in
                    helper newk newterm newsum
                end
    in helper 0 x x end;


(* Question 3 *)

fun integrate f a b n =
    let
        val dx = (b - a) / real n
        fun helper 0 x1 sum = sum
          | helper k x1 sum =
            let
                val x2 = x1 + dx  (* Subinterval is [x1,x2] *)
                val subsum = (f x1 + f x2) / 2.0 * dx  (* Area on subinterval *)
            in
                helper (k - 1) x2 (sum + subsum)
            end
    in helper n a 0.0 end;


(* Question 4 *)

fun variance xs =
    let
        fun helper nil sum f = real sum
          | helper (x::tail) sum f = helper tail (sum + f x) f
        val n = helper xs 0 (fn x => 1)
        val a = helper xs 0 (fn x => x * x) / n  (* First summation *)
        val b = helper xs 0 (fn x => x) / n  (* Second summation *)
    in a - b * b end;
