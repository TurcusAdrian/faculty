(**Exercitiul 1 cu intregi:*)
let x=10;;
let y=15;;
let z=20;;
let min x y = if x<y then x else y;;
let max x y = if x>y then x else y;;
let min3 x y z = min (min x y) z;;
let max3 x y z = max (max x y) z;;
max3 x y z;;
min3 x y z;;

(**Exercitiul 1 cu nr reale (float)*)
let x=10.;;
let y=15.;;
let z=20.;;
let min x y = if x<y then x else y;;
let max x y = if x>y then x else y;;
let min3 x y z = min (min x y) z;;
let max3 x y z = max (max x y) z;;
max3 x y z;;
min3 x y z;;

(**Exercitiul 1 cu siruri de caractere*)
let x="abra";;
let y="cadabra";;
let z="fnhghjsdsbhnj";;
let min x y = if x<y then x else y;;
let max x y = if x>y then x else y;;
let min3 x y z = min (min x y) z;;
let max3 x y z = max (max x y) z;;
max3 x y z;;
min3 x y z;;




(**Exercitiul 2*)
include Printf
let delta a b c= b *. b -. 4. *. a *. c;;
let get_x1 a b c = (-.float_of_int b +. sqrt (float_of_int (delta a b c))) /. (2.0 *. float_of_int a);;
let get_x2 a b c = (-.float_of_int b -. sqrt (float_of_int (delta a b c))) /. (2.0 *. float_of_int a);;
let ec_gr_2 a b c =
  if a == 0 then printf "x = %f\n" ((-.(float_of_int c)) /. (float_of_int b))
  else if (delta a b c) > 0 then printf "x1 = %f, x2 = %f\n" (get_x1 a b c) (get_x2 a b c)
  else if (delta a b c) == 0 then printf "x = %f\n" (get_x1 a b c)
  else printf("Nu exista valori reale ale ecuatiei!\n");;
  ec_gr_2 1 0 (-4);;




  (**Exercitiul 3*)
  include Printf
  let an_bisect x =
  if x mod 4 == 0 then true
  else if x mod 100 == 0 then false
  else if x mod 400 == 0 then true
  else false;;
  an_bisect 2020;;
  an_bisect 2017;;




(**Exercitiul 4 *)
include Printf
let f x y z=
  if x==y && x==z then printf "Toate argumentele sunt egale"
  else
   if x!=y && x!=z && y!=z then printf "Toate argumentele sunt diferite intre ele"
   else
    if x==y && x!=z && y!=z then printf "Argumentele 1 si 2 sunt egale"
    else
     if x==z && x!=y && y!=z then printf "Argumentele 1 si 3 sunt egale"
      else 
       if y==z && x!=y && x!=z then printf "Argumentele 2 si 3 sunt egale";;
f 4 5 5;;
f 2 2 2;;
f 1 3 5;;
f 2 5 2;;
f 1 1 4;;


(**Exercitiul 5*)
include Printf

 let mediana a b c= if (a<b && b<c) || (c<b && b<a) then printf "Mediana e %d\n" b 
                   else 
                   if (b<a && a<c) || (c<a && a<b) then printf "Mediana e %d\n" a
                   else 
                   if (a<c && c<b) || (b<c && c<a) then printf "Mediana e %d\n" c;;
 mediana 3 6 9;;



 (**Exercitiul 6 A*)
 let n f g x = f x + g x;;
 let f x = x + 3;;
 let g x = x + 5;;
 let m = n f g;;
 m 5;;

 (**Exercitiul 6 B*)
 let n f g x = (+) f g;;
 let f x = x + 3;;
 let g x = x + 5;;
 let r= n 2 3;;
 r 10;;