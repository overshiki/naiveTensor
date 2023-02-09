
type 'a naiveTensor = 
          | Tensor of 'a naiveTensor list 
          | Leaf of 'a
          | Null 

let fconcat tx ty = 
    match (tx, ty) with 
      | (Tensor x, Tensor y)  -> Tensor (List.append x y)
      | (Leaf x, Leaf y)      -> Tensor [Leaf x; Leaf y]
      | (Tensor x, Leaf y)    -> Tensor (List.append x [Leaf y])
      | (Leaf x, Tensor y)    -> Tensor (List.append [Leaf x] y)
      | (Null, x)             -> x 
      | (x, Null)             -> x


let range a b = List.init (b-a) ((+) a)

let show_list func (x::xs) = 
  let scat x y = x ^ ", " ^ y in
  let initx = "[" ^ (func x) in
    (List.fold_left scat initx (List.map func xs)) ^ "]\n"


let show_int_list x = show_list string_of_int x


let rec show x = 
    match x with 
      | Tensor xs   -> show_list show xs
      | Leaf x      -> Int.to_string x
      | Null        -> "Null\n"


(* constructors *)
let flattenOne l = Tensor (List.map (fun x->Leaf 1) (range 0 l))
let flattenRange l = Tensor (List.map (fun x->Leaf x) (range 0 l))

let x = Tensor [Leaf 1; Leaf 2]

let () = 
  print_string (show Null);
  print_string (show (Tensor [Leaf 1; Leaf 2]));
  print_string (show x);
  print_string (show_int_list (range 0 10));
  print_string (show (flattenOne 10));
  print_string (show (fconcat (flattenRange 3) (flattenRange 3)));