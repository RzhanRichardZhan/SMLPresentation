(*  _____ _                  _               _  ___  ___ _      *)
(* /  ___| |                | |             | | |  \/  || |     *)
(* \ `--.| |_ __ _ _ __   __| | __ _ _ __ __| | | .  . || |     *)
(*  `--. \ __/ _` | '_ \ / _` |/ _` | '__/ _` | | |\/| || |     *)
(* /\__/ / || (_| | | | | (_| | (_| | | | (_| | | |  | || |____ *)
(* \____/ \__\__,_|_| |_|\__,_|\__,_|_|  \__,_| \_|  |_/\_____/ *)

(* Installation instructions (for a 64 bit machine): *)
(* sudo apt-get install smlnj *)
(* download and upload emac's SML mode here: *)
(* http://www.iro.umontreal.ca/~monnier/elisp/ *)
(* optional: *)
(* sudo apt-get install mlton *)

(* Functional, declarative programming language like Scheme. *)
(* It is not prefix notation. *)
(* Everything must return something (some with side affects like print) *)
(* Typed language; however, type is infered *)



fun cond (x:int):string =    (* ':' indicates type declaration which is optional *)
  if x < 4 then (
      print "Past first\n";
      if x = 3 then
	  "Past second"
      else
	  "Through second"
  )
  else
      "failed";

print(cond(4)^"\n");




fun adder(l:int):int = (* let in end blocks let users declare variables *)
  let                              
      val addie = 5
  in
      l + addie
  end
;

print(Int.toString(adder(9))^"\n");


fun app1 (x,z) =               (* the normal recursive append *)
  if null(x)
  then [z]
  else hd(x)::app1(tl(x),z)
;
fun app2 ([],z) = [z]            (* append with pattern and case *)
  | app2(a::y,z) = a::app2(y,z)
;

(* case (42, false) of *)         (* pattern and case without being in a function *)
(*     (x, true) => x *)
(*   | (23, false) => 17 *)
(*   | _ => 0; *)





fun map (f, []) = []
  | map (f, x1::xs) = (f x1) :: (map(f,xs))
;
map(fn x => 2 * x, [3, 2, 5, 8, 10, 12, 11]);




fun filter (f, []) = nil
  | filter (f, x1::xs) = if f(x1) then x1 :: (filter(f,xs)) else filter(f,xs)
; 
filter(fn x => x mod 2 = 0, [3, 2, 5, 8, 10, 12, 11]);




fun reduce f b nil = b
  | reduce f b (h::t) =  f(h,reduce f b t);
val curryAddReduce = reduce (fn(a,b)=>a+b) 0;
curryAddReduce([3,4,5]);



fun zip f nil nil = nil
  | zip f (h::t) (a::z)  = f(h,a)::zip f t z;
zip (op +) [1,2,6] [3,4,5];



(* type person = int * string list *)
datatype student = id of int
		 | name of string * string;
fun title (id x) = if x div 1000 = 1 then "Senior" else "Inferior"
  | title (name ("Kevin","Zhang")) = "Stuyvesant Student"
  | title (_) = "Not a Stuyvesant Student"
;


(* signatures are equivalent to java's interfaces *)
signature talker =
sig
    type t
    val talk : t -> unit
end

(* structures are little libraries *)
structure foo =
struct
  val talk = fn x => TextIO.print x
end

foo.talk "Hello\n";


