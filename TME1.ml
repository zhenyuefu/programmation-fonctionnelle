(* ========================================== *)
(* == LU2IN019 - FU ZHENYUE == *)
(* == TP 1 == *)
(* ========================================== *)
(* == Question 1 *)
type bit = bool
type duet = bool * bool
type quartet = bool * bool * bool * bool

let xor a b = a!=b
let half_adder a b = (xor a b,a && b)
let adder a b c =
  let s1,r1 = half_adder a b in
  let s2,r2 = half_adder c s1 in
  (s2 , (r1 || r2))
let _=assert ((adder false false false) = (false,false))
let _=assert ((adder false false true) = (true,false))
let _=assert ((adder false true false) = (true,false))
let _=assert ((adder false true true) = (false,true))
let _=assert ((adder true true false) = (false,true))
let _=assert ((adder true true true) = (true,true))

let duet_adder (a1,a2) (b1,b2) r =
  let s1,r1 = adder a2 b2 r in
  let s2,r2 = adder a1 b1 r1 in
  (s2,s1),(r1 || r2)
(* let _=assert ((duet_adder (true,))) *)
