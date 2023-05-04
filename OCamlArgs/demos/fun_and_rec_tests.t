  $ cat << EOF | ./demo.exe -
  > let s = fun s -> fun f -> fun g -> fun x -> f x (g x)
  > let twice = fun x -> (x,x)  
  > ;;
  > EOF
  val s : ('7 -> (('8 -> ('9 -> '10)) -> (('8 -> '9) -> ('8 -> '10)))) = <fun>
  val twice : ('12 -> '12 * '12) = <fun>

  $ cat << EOF | ./demo.exe -
  > let rec map = fun f -> fun l -> match l with 
  > | [] -> []
  > | a::l -> let r = f a in r :: map f l
  > let n = 5 :: 7 :: 9:: []
  > let sq = fun x -> x * x
  > let sqs = map sq n
  > ;;
  > EOF
  val map : (('4 -> '9) -> ('4 list -> '9 list)) = <fun>
  val n : int list = [5; 7; 9]
  val sq : (int -> int) = <fun>
  val sqs : int list = [25; 49; 81]

  $ cat << EOF | ./demo.exe -
  > let rec fix = fun f -> fun eta -> f (fix f) eta
  > let fact =
  > fix (fun fact -> fun n ->
  >  match n with
  >  | 0 -> 1
  >  | m -> m * fact (n - 1))
  > let fact6 = fact 6
  > ;;
  > EOF
  val fix : ((('2 -> '5) -> ('2 -> '5)) -> ('2 -> '5)) = <fun>
  val fact : (int -> int) = <fun>
  val fact6 : int = 720

  $ cat << EOF | ./demo.exe - 
  > let mul = fun a -> fun b -> a * b
  > ;;
  > EOF
  val mul : (int -> (int -> int)) = <fun>

  $ cat << EOF | ./demo.exe -
  > let rec map = fun f -> fun l -> match l with 
  > | [] -> []
  > | a::l -> let r = f a in r :: map f l
  > let n = 5 :: 7 :: 9:: []
  > let sq = fun x -> x * x
  > let sqs = map sq n
  > ;;
  > EOF
  val map : (('4 -> '9) -> ('4 list -> '9 list)) = <fun>
  val n : int list = [5; 7; 9]
  val sq : (int -> int) = <fun>
  val sqs : int list = [25; 49; 81]


  $ cat << EOF | ./demo.exe -
  > let id = fun x -> x
  > 
  > let rec map = fun f -> fun xs ->
  >   match xs with
  >   | [] -> []
  >   | x :: xs -> f x :: map f xs
  > ;;
  > 
  > let id_map = map id
  > let abs_map = map (fun x -> match x < 0 with | true -> -x | false -> x)
  > let list = abs_map (1 :: 2 :: -3 :: 0 :: -8 :: [])
  > ;;
  > EOF
  val id : ('1 -> '1) = <fun>
  val map : (('6 -> '11) -> ('6 list -> '11 list)) = <fun>
  val id_map : ('16 list -> '16 list) = <fun>
  val abs_map : (int list -> int list) = <fun>
  val list : int list = [1; 2; 3; 0; 8]
