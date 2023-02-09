
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

let rec show x = 
    match x with 
      | Tensor xs   -> (List.fold_left ( ^ ) "[" (List.map show xs)) ^ "]"
      | Leaf x      -> Int.to_string x
      | Null        -> "Null"

let x = Tensor [Leaf 1; Leaf 2]

let () = 
  print_string "Hello world!\n";
  print_string (show Null);
  print_string "\n";
  print_string (show (Tensor [Leaf 1; Leaf 2]));
  print_string "\n";
  print_string (show x);
  print_string "\n";