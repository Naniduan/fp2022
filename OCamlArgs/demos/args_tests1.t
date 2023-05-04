  $ cat << EOF | ./demo.exe -
  > let ratio = fun ~num -> fun ~den -> num / den
  > let a = ratio ~den:2
  > let c = ratio ~num:4
  > let b = ratio ~den:2 ~num:4
  > let d = ratio ~num:4 ~den:2
  > ;;
  > EOF
  val ratio : (~num:int -> (~den:int -> int)) = <fun>
  val a : (~num:int -> int) = <fun>
  val c : (~den:int -> int) = <fun>
  val b : int = 2
  val d : int = 2


  $ cat << EOF | ./demo.exe -
  > let ratio_caller = fun ratio -> ratio ~den:2 ~num:4
  > let ratio = fun ~num -> fun ~den -> num / den
  > let x = ratio_caller ratio
  > ;;
  > EOF
  val ratio_caller : ((~den:int -> (~num:int -> '3)) -> '3) = <fun>
  val ratio : (~num:int -> (~den:int -> int)) = <fun>
  val x : int = 2

  $ cat << EOF | ./demo.exe -
  > let fst = fun a -> fun ?b -> a
  > let x = fst 1
  > ;;
  > EOF
  val fst : ('2 -> (?b:(('1) option) -> '2)) = <fun>
  val x : (?b:(('1) option) -> int) = <fun>

  $ cat << EOF | ./demo.exe -
  > let scd = fun ?a -> fun b -> b
  > let x = scd 2
  > ;;
  > EOF
  val scd : (?a:(('0) option) -> ('2 -> '2)) = <fun>
  val x : int = 2

  $ cat << EOF | ./demo.exe -
  > let trd = fun ?a -> fun ?b -> fun c -> c
  > let x = trd 3
  > ;;
  > EOF
  val trd : (?a:(('0) option) -> (?b:(('1) option) -> ('3 -> '3))) = <fun>
  val x : int = 3

  $ cat << EOF | ./demo.exe -
  > let fst = fun a -> fun ?b -> a
  > let x = fst 1 ?b:(Some 2)
  > ;;
  > EOF
  val fst : ('2 -> (?b:(('1) option) -> '2)) = <fun>
  val x : int = 1

  $ cat << EOF | ./demo.exe -
  > let sum = fun ?a -> fun b -> 
  >   match a with
  >   | Some a -> a + b
  >   | None -> b
  > let x = sum 1
  > ;;
  > EOF
  val sum : (?a:(('0) option) -> (int -> int)) = <fun>
  val x : int = 1

  $ cat << EOF | ./demo.exe -
  > let sum = fun ?a -> fun b -> 
  >   match a with
  >   | Some a -> a + b
  >   | None -> b
  > let x = sum 1 ?a:(None)
  > ;;
  > EOF
  val sum : (?a:(('0) option) -> (int -> int)) = <fun>
  val x : int = 1

  $ cat << EOF | ./demo.exe -
  > let sum = fun ?a -> fun b -> 
  >   match a with
  >   | Some a -> a + b
  >   | None -> b
  > let y = sum ?a:(Some 1) 2
  > let z = sum 3 ?a:(Some 4)
  > ;;
  > EOF
  val sum : (?a:(('0) option) -> (int -> int)) = <fun>
  val y : int = 3
  val z : int = 7

  $ cat << EOF | ./demo.exe -
  > let sum = fun ?a -> fun b -> 
  >   match a with
  >   | Some a -> a + b
  >   | None -> b
  > let w = (sum 3) ?a:(Some 4)
  > ;;
  > EOF
  Error: unification failed on int and (?a:((int) option) -> '7)
