  $ cat << EOF | ./demo.exe -
  > let six_values = fun a -> fun ?b -> fun ~c -> fun ~d -> fun ?e -> fun f ->
  > match b, e with
  >   | Some s1, Some s2 -> (a, s1, c, d, s2, f)
  >   | Some s1, None -> (a, s1, c, d, 0, f)
  >   | None, Some s2 -> (a, 0, c, d, s2, f)
  >   | None, None -> (a, 0, c, d, 0, f)
  > let x = six_values 1 6
  > ;;
  > EOF
  val six_values : ('15 -> (?b:(('1) option) -> (~c:'16 -> (~d:'17 -> (?e:(('4) option) -> ('18 -> '15 * int * '16 * '17 * int * '18)))))) = <fun>
  val x : (?b:(('1) option) -> (~c:'29 -> (~d:'30 -> (?e:(('4) option) -> int * int * '29 * '30 * int * int)))) = <fun>

  $ cat << EOF | ./demo.exe -
  > let trd_plus_n = fun ?a -> fun ?b -> fun c -> fun ~n -> c + n
  > let x = trd_plus_n 3 ?a:(Some 1) ?b:(Some 2) ~n:4
  > ;;
  > EOF
  val trd_plus_n : (?a:(('0) option) -> (?b:(('1) option) -> (int -> (~n:int -> int)))) = <fun>
  val x : int = 7

  $ cat << EOF | ./demo.exe -
  > let trd_plus_n_and_m = fun ?a -> fun ?b -> fun c -> fun ~n -> fun ~m -> c + n + m
  > let x = trd_plus_n_and_m 3 ?a:(Some 1) ?b:(Some 2) ~n:4 ~m:3
  > ;;
  > EOF
  val trd_plus_n_and_m : (?a:(('0) option) -> (?b:(('1) option) -> (int -> (~n:int -> (~m:int -> int))))) = <fun>
  val x : int = 10

  $ cat << EOF | ./demo.exe -
  > let trd_and_frt_plus_n = fun ?a -> fun ?b -> fun c -> fun d -> fun ~n -> c + n + d
  > let x = trd_and_frt_plus_n 3 ?a:(Some 1) ?b:(Some 2) ~n:4 3
  > ;;
  > EOF
  val trd_and_frt_plus_n : (?a:(('0) option) -> (?b:(('1) option) -> (int -> (int -> (~n:int -> int))))) = <fun>
  val x : int = 10

  $ cat << EOF | ./demo.exe -
  > let all = fun ?a -> fun ?b -> fun c -> fun d -> fun ~n -> fun ~m -> c + n + d + m
  > let x = all 3 ?a:(Some 1) ?b:(Some 2) ~n:4 3 ~m:5
  > ;;
  > EOF
  val all : (?a:(('0) option) -> (?b:(('1) option) -> (int -> (int -> (~n:int -> (~m:int -> int)))))) = <fun>
  val x : int = 15

  $ cat << EOF | ./demo.exe -
  > let another = fun ~d -> fun ~e -> fun f -> d + f
  > let x = another 4 ~e:1 ~d:5
  > ;;
  > EOF
  val another : (~d:int -> (~e:'3 -> (int -> int))) = <fun>
  val x : int = 9

  $ cat << EOF | ./demo.exe -
  > let sum_of_all = fun ~n -> fun a -> fun ?x -> fun ~m -> fun b -> fun ?y -> fun c -> fun ~nm -> fun ?z -> fun d -> a + b + c + d + n + m + nm
  > let x = sum_of_all ~nm:7 1 ~n:5 2 3 ~m:6 4
  > ;;
  > EOF
  val sum_of_all : (~n:int -> (int -> (?x:(('2) option) -> (~m:int -> (int -> (?y:(('5) option) -> (int -> (~nm:int -> (?z:(('8) option) -> (int -> int)))))))))) = <fun>
  val x : int = 28
