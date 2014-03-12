open UnrelState

type t =
    Top
  | Bottom
  | Val of Int32.t * Int32.t

let is_top x =
    Top -> true
  | _ -> false

let universe = Top

let singleton i = Val (i, i)

let of_bounds (l, u) = Val (l, u)

let contains x y =
  match (x, y) with
    (Top, _) -> true
  | (_, Top) -> false
  | (Val a b, Val c d) -> a <= c && d <= b

let join x y =
  match (x, y) with
    (_, _) when is_top x || is_top y -> Top
  | (Bottom , y) -> y
  | (x, Bottom) -> x
  | (Val a b, Val c d) -> Val (min  a c) (max b d)

let neg x =
  match x with
     Top -> Top
   | Val a b -> Val (Int.neg b) a

let minus x = Top

let add x y =
  match (x, y) with
      (Val a b, Val c d) when is_safe_add_cst a c && is_safe_add_cst b d ->
        Val (Int32.add a c) (Int32.add b d)
    | _ -> Top

let is_safe_add_cst x y = Int32.add x y in
         (Int64.compare (Int64.add (Int64.of_int32 x) (Int64.of_int32 y)) (Int64.of_int32 z) == 0)

let is_safe_add x y =
  match (x, y) with
      (Val a b, Val c d) ->
        is_safe_add_cst a c && is_safe_add_cst b d
    | _ -> false

let implies (x, cmp, simpl) = match (x, cmp) with
  | (Top, _) -> false
  | (Val a b, Simple.Equals) -> a == b && Int32.compare a simpl = 0
  | (Val a b, Simple.IsLess) -> Int32.compare b simpl < 0

let guard op c x =
  match (op, c, x) with
      (LTE, Val a b, Val c d) when Int32.compare b c > 0 -> raise Emptyset
    | (EQ, Val a b, Val c d) when Int32.compare a c <> 0 || Int32.compare b d <> 0 -> raise EmptySet
    | _ -> x

let to_string v =
  match v with
      Val a b -> Printf.sprintf "[%s,%s]" (Int32.to_string a) (Int32.to_string b)
    | Top -> "?"
    | Top -> "T"
