


(* Exercice 1 *)

let rec fact (n : int) : int =
  if (n = 0) then 1
else n*(fact (n - 1) )

let rec fact2 (n : int) : int =
if (n<0) raise (Invalid_argument "fact2: Negative argument")
else if (n=0) then 1
else n * (fact2 (n-1))

(* Pb de la version ci-dessus : le test if (n<0) est fait à chaque
   appel récursif *)

let fact3 (n : int) : int =
let rec faux (n : int) : int =
  if (n = 0) then 1
  else n * (faux  (n - 1))
in
if (n < 0) then raise (Invalid_argument "fact2: Negative argument")
else (faux n)

(* fact3 ne fait qu'un seul test pour voir si l'argument est négatif *)

(* exemple d'appel de fact

   (fact 5) =
   5 * (fact 4) =
   5 * 4 * (fact 3) =
   5 * 4 * 3 * (fact 2) =
   5 * 4 * 3 * 2 * (fact 1) =
   5 * 4 * 3 * 2 * 1 * (fact 0) =
   5 * 4 * 3 * 2 * 1 * 1

   Les calculs en attente sont stockées dans la pile (= stack en anglais)
   La pile peut déborder : stack overflow
   La récurrence terminale est une méthode qui évite de stocker des choses dans la pile
   *)

let fact4 (n : int) : int =
let rec faccu (na : int) (accu : int) : int =
if (na = 0)
  then accu
  else (faccu (na-1) (na * accu))
in
(faccu n 1)

(* Exemple d'appel de fact4
   (fact4 5) =
   (faccu 5 1) =
   (faccu 4 5) =
   (faccu 3 20) =
   (faccu 2 60) =
   (faccu 1 120) =
   (faccu 0 120) =
   120

 fact4 est une fonction récursive terminale car on ne stocke rien dans la pile
   *)


(* exemple de définition locale

   let expr_locale = valeur_locale in expr_globale *)

let x = 2 in
x+1


(* Exercice 2 *)

(*Q1 *)

(* version récursive non terminale *)
(* Hypothèse : n > 0 *)
let rec sum_n (n : int) : int =
if (n = 1)
  then 1
  else n + (sum_n (n-1))

(*Version récursive terminale *)
let sum_n2 n =
let rec saccu n acc =
  if (n = 1) then acc
  else (saccu (n-1) (n+acc))
in
(saccu n 1)

(*Q2*)

let sum_n (n : int) : int =
let rec saux n =
  if (n=1) then 1
  else n+(saux (n-1))
in
if (n <= 0) then raise (Invalid_argument "sum_n: Negative argument")
else (saux n)

(* Q3 *)

let rec sum_p (n : int) : int =
if (n = 0) then 0
else 2*n + (sum_p (n-1))

(* Ex d'exécution
   (sum_p 4) =
   8 + (sum_p 3) =
   8 + 6 + (sum_p 2) =
   8 + 6 + 4 + (sum_p 1) =
   8 + 6 + 4 + 2 + (sum_p 0) =
   8 + 6 + 4 + 2 + 0

   *)

(* Variante *)

let rec sum_p (n : int) : int =
if (n = 0) then 0
else if (n %2 = 0)
   then n + (sum_p (n - 1))
   else (sum_p (n -1))

(* Q4 *)

let rec sum_f (f : int -> int) (n : int) : int =
if ( n = 0 )
then (f 0)
else (f n) + (sum_f f (n-1))

(* Ex avec f = fun x -> x+1 et n = 4
   (sum_f f 4) =
   (f 4) + (sum_f f 3) =
   5     + (f 3) + (sum_f f 2) =
   5     + 4     + (f 2) + (sum_f f 1) =
   5     + 4     + 3     + (f 1) + (sum_f f 0) =
   5     + 4     + 3     + 2     + (f 0) =
   5     + 4     +3      + 2     + 1

   *)

let sum_p (n : int) : int =
let f2 n = 2 * n
in
sum_f f2 n

(* Exercice 3 *)

(* Q1 *)

let rec u (n: int ) : int =
if (n = 0) then 42
else 3* (u (n-1)) + 4

(* Version récursive terminale *)
let u2 (n : int) : int =
let rec uaux n accu =
  if (n = 0) then accu
  else (uaux (n-1) (3*accu +4))
in
(uaux n 42)

(* Ex d'exécution
   (u 4) =
   (uaux 4 42) =
   (uaux 4-1 3*42 +4)=
   (uaux 3 130) =
   (uaux 3-1 3*130 + 4) 
   (uaux 2 394) =
   (uaux 1 3* 394 + 4)=
   (uaux 1 1186) =
   (uaux 0 3*1186+4) =
   (uaux 0 3562)
   = 3562
   

   *)

(* Variante où on fait grandir l'argument de la fonction auxillaire)

let u3 n =
let rec uaux i accu = 
  if i =n then accu
  else (uaux (i+1) (3*accu+4))
in
(uaux 0 42)
