(*課題１*)
(*mergeソート*)
(*リストを最小限に分割する*)
let rec split lst =
  match lst with
    [] -> ([], [])
    | hd1 :: [] -> (hd1 :: [], [])
    | hd1 :: hd2 :: tl -> let (x,y) = split tl 
                                    in (hd1 :: x , hd2 :: y)

(*最小限に分割したリストを並べ替えて併合していく*)
let rec merge (lst1, lst2) =
    match (lst1, lst2) with
       (_, []) -> lst1
      |([], _) -> lst2
      |(first1 :: rest1) , (first2 :: rest2) -> 
                             if first1 < first2 then first1 :: merge (rest1, lst2)
                             else first2 :: merge (lst1, rest2)

(*二つの関数を組み合わせて完成させる*)
let rec mergesort ls =
    match ls with
       [] -> []
      |[a] -> [a]
      |_ -> let (b, c) = split ls
                       in merge (mergesort (b), mergesort (c))

(*挿入ソート*)
let rec insert x lst =
    match lst with
       [] -> [x]
      | first :: rest -> if x <= first then x :: first :: rest
                  else first :: insert x rest
let rec insertsort lst =
    match lst with
         [] -> []
        |head :: tale -> insert head (insertsort tale)

(*実行速度を比較するために作成した任意の数の要素のリストを作る関数*)
let rec makelst n =
    match n with 
       1 -> [1]
      |_ -> n :: (makelst (n-1))       

(*実行速度を比較するために作成した関数makelstを逆順にする関数*)    
let rec append l1 l2 =
match l1 with
[] -> l2
| first :: rest -> first :: (append rest l2)

let rec reverse lst =
match lst with
[] -> []
| first :: rest -> append(reverse rest) [first]

(*実行時間の測定に使う関数*)
(*let test1 = reverse (makelst 1000)*)

(*課題３*)
let rec kaidan n =
    match n with
       1 -> 1
      |2 -> 2
      |_ -> kaidan (n-1) + kaidan (n-2)

(*実行速度を比較するために作成した線形な再帰を利用した関数*)
let kaidan2 n = 
    let rec kaipair n =
        if n = 1 then (1, 1)
        else
            let (i, j) = kaipair (n - 1)
            in (i + j, i)
    in let (i, _) = kaipair n
    in i

(*課題４*)
let doukannsuu f x =
  let h = 0.1e-5
in ((f(x +. h) -. f(x)) /. h )

let bibunn f =
  let h = 0.1e-6
in fun x -> ((f(x +. h) -. f(x)) /. h )

let newton f =
let rec sub x1 n =
   match n with
      0 -> x1
     |_ -> sub (x1 -. ((doukannsuu f x1) /. (doukannsuu (bibunn f) x1))) (n-1)
in sub 10. 10000

let ext f = f (newton f)

(*実行速度を比較するために作成した関数*)
let e x = 2. *. x *. x -. 2. *. x

(*課題５*)
(*台形の面積を求める関数*)
let daikei jyoutei katei takasa =
   (jyoutei +. katei) *. takasa /. 2.

(*台形の面積を用いて定積分を近似する*)
let sekibunn f a b =
   let takasa = (b -. a) /. 100.
in let rec siguma n =
       match n with
          0. -> 0.
         |_ -> daikei (f (a +. takasa *. (n -. 1.))) (f (a +. takasa *. n)) takasa 
               +. siguma (n -. 1.)     
in siguma 100.

let sekibunn2 f a b =
   let takasa = (b -. a) /. 10.
in let rec siguma n =
       match n with
          0. -> 0.
         |_ -> daikei (f (a +. takasa *. (n -. 1.))) (f (a +. takasa *. n)) takasa
               +. siguma (n -. 1.)
in siguma 10.

let sekibunn3 f a b =
   let takasa = (b -. a) /. 10000.
in let rec siguma n =
       match n with
          0. -> 0.
         |_ -> daikei (f (a +. takasa *. (n -. 1.))) (f (a +. takasa *. n)) takasa
               +. siguma (n -. 1.)
in siguma 10000.

(*課題６*)    
let rec collatz n =
      if  n = 1 then 1
      else if (n mod 2= 0) then collatz (n / 2)
      else collatz ((n * 3) + 1)

(*実行速度を比較するために作成した関数*)
let rec collatz2 n =
       if  n = 1 then 1 :: []
      else if (n mod 2= 0) then n :: collatz2 (n / 2)
      else n :: collatz2 ((n * 3) + 1)

let rec length lst =
match lst with
[] -> 0
| _ :: rest -> 1 + (length rest)

(*ステップ数を比較するための関数*)
let kaisuu n = length (collatz2 n)
