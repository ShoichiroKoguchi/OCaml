(*１．受け取ったリストに０があるか調べる*)
let rec checkl n lst =
      match lst with
       [] -> false
       |first::rest -> if first = n then true
                       else checkl n rest

(*２．受け取った整数値の分リストの先頭から要素を削除する*)
(*int -> int list*)
let rec dellt n lst =
      if n < 0 then failwith "Error"
      else
       match n , lst with 
       _ , [] -> []
       |0 , _ -> lst
       |_ , _::rest -> dellt (n-1) rest



(*３．受け取った整数値番目の要素を削除する*)
let rec dellt2 n lst =
       match (n , lst) with
       (_ , []) -> []
       |(0 , _) -> lst
       |(1 , first::rest) -> dellt2 (n-1) rest
       |(_ , first::rest) -> first :: (dellt2 (n-1) rest)

(*4．受け取った整数値番目の要素を取り出す*)
(*int -> 'a*)
let rec posl n lst =
    match n , lst with
       0 , _ -> failwith "Not Exist"
       |_ , [] ->failwith "Not Exist"
       |_ , first :: rest -> if n=1 then first
                         else posl (n-1) rest

(*５、n番目とn+1番目の和を計算してリストを再構成する*)
(*int list -> int list*)
let rec add2list lst =
    match lst with
      [] -> []
      |first :: rest ->  match rest with
                           |[] -> []
                           |hd :: tl -> (first + hd) :: add2list rest

(*6.*)
let rec mullist lst1 lst2 =
     match lst1 , lst2 with
       [] , _ -> []
       |_ , [] -> []
       |hd1 :: tl1 , hd2 :: tl2 -> (hd1*hd2) :: mullist tl1 tl2

(*7.*)
let rec chglist (n,m) lst =
     match lst with
       [] -> []
       |first :: rest -> if first = n then m :: chglist (n,m) rest
                         else first :: chglist (n,m) rest
       


(*8.リストを受け取った整数倍する*)
let replicate n unit =
 let rec rep n x =
      if n = 0 then x
      else rep (n-1) (unit::x)
 in
    rep n []

(*9. 整数値番目に受け取った文字列を挿入する*)
(*int -> 'a list*)
let rec inslist n str lst =
     match n , lst with
      0, _ -> failwith "Error"
      |1 , first :: rest -> str :: first :: rest
      |_ , first :: rest -> first :: (inslist (n-1) str rest)
      |1 , [] -> str :: []
      |_ , [] -> failwith "Error" 

(*10. 2つの要素を要素順に結合する*)
(*'a list -> 'a list*)
let rec merge lst1 lst2 =
       match lst1 , lst2 with
       [] , _ -> lst2
       |_ , [] -> lst1
       |hd1 :: tl1 ,hd2 :: tl2 -> hd1 :: hd2 :: merge tl1 tl2    

(*11.複数のリストの要素数を求める*)
(*'a list list -> int*)
                    
let rec inside_length lst =
     match lst with
     [] -> 0
     |first :: rest -> let rec inside first =
                       match first with
                       |[] -> 0
                       |hd :: tl -> 1+ inside tl
in inside first + inside_length rest

(*12.複数のリストを一つのリストにまとめる*)
let rec concat lst =
  match lst with 
    [] -> []
    | first :: _ when first = [] -> []
    | first :: rest -> first @ concat rest  

(*13*)  
let rec assoc n lst =
  match lst with
    |[] -> failwith "Not found..."
    | first :: rest -> match first with 
                          (a , b)  -> if a = n then b 
                                      else if b = n then a
                                      else assoc n rest
(*14.リストの中の最小値を求める*)
(*'a list ->a*)       
let minimum lst =
  let rec min a = function
    [] -> a
    |hd :: lst -> if hd < a then min hd lst
                 else min a lst
     in
     match lst with
      [] -> failwith "Error"
      |first::rest -> min first rest

(*15.*)
let rec extract f =function
      [] -> []
      |first :: rest -> if f first then first :: extract f rest
                        else extract f rest

(*16.9c4を求める*)
let numOfRotes (n,m) =
 let x = n + m in
 let rec sub (x,y) =
  if y = 0 then 1
  else (sub (x,y-1)) * (x-y+1) / y
 in
  if  n <= m then sub (x,n)
  else sub (x,m)
