(* ========================================== *)
(* == LU2IN019 - FU ZHENYUE == *)
(* == TP 1 == *)
(* ========================================== *)
type bit = bool
type duet = bit * bit
type quartet = bit * bit * bit * bit

let xor (a:bit) (b:bit):bit= a<>b
let half_adder  (a:bit) (b:bit):(bit*bit)= (xor a b,a && b)
let adder (a:bit) (b:bit) (c:bit):(bit*bit) =
  let s1,r1 = half_adder a b in
  let s2,r2 = half_adder c s1 in
  (s2 , (r1 || r2))
let _=assert ((adder false false false) = (false,false))
let _=assert ((adder false false true) = (true,false))
let _=assert ((adder false true false) = (true,false))
let _=assert ((adder false true true) = (false,true))
let _=assert ((adder true true false) = (false,true))
let _=assert ((adder true true true) = (true,true))


let duet_adder ((a1,a2):duet) ((b1,b2):duet) (r:bit):(duet*bit) =
  let s1,r1 = adder a2 b2 r in
  let s2,r2 = adder a1 b1 r1 in
  (s2,s1),r2

let _=assert ((duet_adder (true,false) (true,false) false) = ((false,false),true))

let quartet_adder ((a1,a2,a3,a4):quartet) ((b1,b2,b3,b4):quartet) (r:bit):(quartet*bit) =
  let (s3,s4),r1 = duet_adder (a3,a4) (b3,b4) r in
  let (s1,s2),r2 = duet_adder (a1,a2) (b1,b2) r1 in 
  (s1,s2,s3,s4),r2

let to_quartet (n:int):quartet = 
  let i2b i = if i=0 then false else true in
  let r4 = i2b (n mod 2) in
  let n = n / 2 in 
  let r3 = i2b (n mod 2) in
  let n = n / 2 in
  let r2 = i2b (n mod 2) in
  let r1 = i2b (n / 2) in
  (r1,r2,r3,r4)