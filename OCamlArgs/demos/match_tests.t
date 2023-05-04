  $ cat << EOF | ./demo.exe -
  > let n = match 0 with
  >  | 0 -> 1000
  >  | m -> m - 1
  > ;;
  > EOF
  val n : int = 1000

  $ cat << EOF | ./demo.exe -
  > let n = match Some 1 with
  >  | Some a -> a
  >  | None -> 1000
  > ;;
  > EOF
  val n : int = 1

  $ cat << EOF | ./demo.exe -
  > let n = match None with
  >  | None -> 1000
  >  | Some a -> a
  > ;;
  > EOF
  val n : int = 1000

