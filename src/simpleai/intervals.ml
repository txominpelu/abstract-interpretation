open UnrelState

type t =
    Top
  | Bottom
  | Val of Int32.t * Int32.t

let is_top = function
    Top -> true
  | _ -> false

let universe = Top

let singleton i = Val (i, i)

let of_bounds (l, u) = Val (l, u)

let contains x y =
  match (x, y) with
  | (Top, _) -> true
  | (_, Top) -> false
  | (Val (a, b), Val (c, d)) -> a <= c && d <= b

let join x y =
  match (x, y) with
    (Top, _) -> Top
  | (_, Top) -> Top
  | (Bottom , y) -> y
  | (x, Bottom) -> x
  | (Val (a, b), Val (c, d)) -> Val ((min  a c), (max b d))

let neg x =
  match x with
   | Top -> Top
   | Val(a, b) -> Val ((Int32.neg b), (Int32.neg a))

let is_safe_add_cst x y =
    let z = Int32.add x y in
    (Int64.compare (Int64.add (Int64.of_int32 x) (Int64.of_int32 y)) (Int64.of_int32 z) == 0)

let is_safe_minus_cst x y =
    let z = Int32.add x (Int32.neg y) in
    (Int64.compare (Int64.add (Int64.of_int32 x) (Int64.neg (Int64.of_int32 y))) (Int64.of_int32 z) == 0)

let widen x y =
  match (x, y) with
    (Val (a, b), Val (c, d)) ->
      let e = if a <= c then a else Int32.min_int in
      let f = if b >= d then b else Int32.max_int in
      Val (e, f)
    | (_, _)  -> Val (Int32.min_int, Int32.max_int)

let minus x y =
  match (x, y) with
    | (Val (a, b), Val(c, d)) when is_safe_add_cst a  (Int32.neg d) && is_safe_add_cst b (Int32.neg c) ->
        Val ((Int32.add a (Int32.neg d)), (Int32.add b (Int32.neg c)))
    | _ -> Top

let min_max f x y=
  match (x, y) with
    | (Val (a, b), Val(c, d)) ->
        let pairs = [(a , c); (a , d); (b , c); (b , d);] in
        let products = List.map (fun (x, y) -> f x y) pairs in
        let newa = List.fold_left (fun acc -> fun v -> min acc v)
        Int32.max_int products in 
        let newb = List.fold_left (fun acc -> fun v -> max acc v)
        Int32.min_int products in
        Val (newa, newb)
    | _ -> Top

let mult x y = min_max Int32.mul x y

let div x y = min_max Int32.div x y

let add x y =
  match (x, y) with
    | (Val (a, b), Val(c, d)) when is_safe_add_cst a c && is_safe_add_cst b d ->
        Val ((Int32.add a c), (Int32.add b d))
    | _ -> Top

let is_safe_minus x y =
  match (x, y) with
    | (Val (a, b), Val(c, d))->
        is_safe_minus_cst a d && is_safe_minus_cst b c
    | _ -> false

let is_safe_add x y =
  match (x, y) with
    | (Val (a, b), Val(c, d))->
        is_safe_add_cst a c && is_safe_add_cst b d
    | _ -> false

let is_safe f64 f32 x y =
  match (x, y) with
    | (Val (a, b), Val(c, d))->
        let pairs = [(a , c); (a , d); (b , c); (b , d);] in
        List.for_all (fun v -> 
          let (x, y) = v in
          let z = f32 x y in
          (Int64.compare (f64 (Int64.of_int32 x) (Int64.of_int32 y)) (Int64.of_int32 z) == 0)
        ) pairs
    | _ -> false

let is_safe_mult x y =
    is_safe Int64.mul Int32.mul x y

let is_safe_div x y =
  let eq_zero x = Int32.compare x Int32.zero == 0 in
  let gte_zero x = Int32.compare x Int32.zero >= 0 in
  let lte_zero x = Int32.compare x Int32.zero <= 0 in
  match (x, y) with
    | (Val (a, b), Val(c, d))->
        is_safe Int64.div Int32.div x y 
           && not (eq_zero c)
           && not (eq_zero d)
           && not (lte_zero c && gte_zero d)
    | _ -> false

let implies (x, cmp, simpl) = match (x, cmp) with
  | (Top, _) -> false
  | (Bottom, _) -> true
  | (Val (a, b), Simple.Equals) -> a == b && Int32.compare a simpl = 0
  | (Val (a, b), Simple.IsLess) -> Int32.compare b simpl < 0

let guard op c x =
  match (op, c, x) with
    | (LTE, Val(a, b), Val(c, d)) when Int32.compare b c > 0 -> raise Emptyset
    | (EQ, Val(a, b), Val(c, d)) when Int32.compare a c <> 0 || Int32.compare b d <> 0 -> raise Emptyset
    | (LT, Val(a, b) , Val(e, f)) when e >= b -> c
    | (LT, Val(a, b) , Val(e, f) ) when e > a && e < b -> Val (a, Int32.add (Int32.neg Int32.one) e)
    | (LT, Val(a, b) , Val(e, f) ) when e <= a -> Bottom
    | (GT, Val(a, b) , Val(e, f)) when f <= a -> c
    | (GT, Val(a, b) , Val(e, f) ) when f > a && f < b -> Val (f, b)
    | (GT, Val(a, b) , Val(e, f) ) when f = a && f < b -> Val (Int32.add Int32.one a, b)
    | (GT, Val(a, b) , Val(e, f) ) when e >= a -> Bottom
    | (GTE, Val(a, b) , Val(e, f)) when f <= a -> c
    | (GTE, Val(a, b) , Val(e, f) ) when f >= a && f < b -> Val (f, b)
    | (GTE, Val(a, b) , Val(e, f) ) -> Bottom
    | (LTE, Val(a, b) , Val(e, f)) when e >= b -> c
    | (LTE, Val(a, b) , Val(e, f) ) when e >= a -> Val (a, min e b)
    | (LTE, Val(a, b) , Val(e, f) ) when e < a -> Bottom
    | _ -> x

let string_of_int32 = function
    | a when Int32.compare a Int32.min_int = 0 -> "INT_MIN"
    | a when Int32.compare a Int32.max_int = 0 -> "INT_MAX"
    | a -> Int32.to_string a

let to_string v =
  match v with
    | Val (a, b) ->
      Printf.sprintf "[%s,%s]" (string_of_int32 a) (string_of_int32 b)
    | Bottom -> "?"
    | Top -> "T"
