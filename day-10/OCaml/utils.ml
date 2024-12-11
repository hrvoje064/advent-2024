(* File reading tools - utilities *)

(* tools *)
(* =========================================== *)
(* timing functions *)

let time thunk =
  Gc.full_major ();
  let t = Sys.time() in
  let result = thunk () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  result

let time_opt thunk =
  Gc.full_major ();
  Gc.full_major ();
  let t = Sys.time() in
  let (a,b) = thunk () in
  let (t,p1,p2) =
    (Printf.sprintf "Run time: %fs\n" (Sys.time () -. t), a, b) in
  (t,p1,p2)

let time_void thunk =
  Gc.full_major ();
  let t = Sys.time() in
  thunk ();
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t)

let take_drop n l =
  let rec t_d n l =
    match n,l with
      0,l -> ([],l)
    | _,[] -> ([],[])
    | n,x::xs ->
      match t_d (n - 1) xs with
        (take,drop) -> (x :: take, drop)
  in if n < 0 then failwith (invalid_arg "take_drop")
  else t_d n l

let take n l = fst (take_drop n l)
let drop n l = snd (take_drop n l)
    
let take_drop_e l n =
  let rec t_d l n =
    match l,n with
      l,0 -> ([],l)
    | x::xs,n ->
      begin
        match t_d xs (n-1) with t,d -> (x::t,d)
      end
    | [],n -> failwith (invalid_arg "take_drop_e")
  in if n < 0
  then failwith (invalid_arg "take_drop_e")
  else t_d l n

let take_e l n = fst (take_drop_e l n)
let drop_e l n = snd (take_drop_e l n)

(* compose *)
let (<<) f g x = f (g x)

(* string to list *)
let explode s =
  List.init (String.length s) (String.get s)
(*list to string *)  
let implode l = l |> List.to_seq |> String.of_seq

(* interval from -- to *)
let ( -- ) a b = List.init (b-a+1) (fun i -> (i+a))

let is_int s =
  try int_of_string s |> ignore; true
  with Failure _ -> false

let is_float s =
  try float_of_string s |> ignore; true
  with Failure _ -> false
   
let is_number s =
  try
    int_of_string s |> ignore ; true
  with Failure _ ->
  try
    float_of_string s |> ignore; true
  with Failure _ -> false

let rec remove e = function
    [] -> []
  | x::xs -> if e = x then xs
    else x :: remove e xs

let rec remove_all e = function
    [] -> []
  | x::xs -> if e = x then remove_all e xs
    else x :: remove_all e xs

(* returns a tuple of lists *)
let rec filter_split p = function
    [] -> ([],[])
  | x::xs -> match filter_split p xs with
      (t, f) -> if (p x) then (x :: t, f)
      else (t, x :: f)

let even n = n mod 2 = 0
let odd = not << even

(* char_to_int '7' => 7 *)
let char_to_int c = int_of_string (Char.escaped c)
    
(* ============================================ *)
(* File Tools *)
(* =============================================== *)
    
(* read the entire file as a string *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all

(* read lines - returns list of strings *)
let read_lines file =
  In_channel.with_open_bin file In_channel.input_lines

(* read and process line by line - my generic function *)
let read_process_lines f file =
  let rec read ic =
     match In_channel.input_line ic with
       None -> []
     | Some l -> f l :: read ic
  in In_channel.with_open_bin file read

(* read and process line by line - my generic function
   copy of Scheme function read-lol-words:
   read a file into list of lists of strings
   splits every string (from read-lines) into a list of
   word strings split on space - extra spaces removed *)
let read_lol_words file =
  let rec read ic =
    match In_channel.input_line ic with
      None -> []
    | Some l ->
      List.filter ((<>) "") (String.split_on_char ' ' l) :: read ic
  in In_channel.with_open_bin file read

(* filter string - my - much faster than converting to seq*)
let string_filter_b bl p s =
  let bcc = Buffer.create bl in
  String.fold_left (fun bcc c ->
      if p c
      then begin Buffer.add_char bcc c; bcc end
      else bcc)
     bcc s |> Buffer.contents

(* filter_string - my, slower than buffer, a bit faster than lib - seq *)
let string_filter p s = 
  String.fold_right (fun c acc ->
      if p c
      then (Char.escaped c) ^ acc
      else acc)
     s ""

(* filter string - lib - seq *)
let string_seq_filter p s = (* slower *)
  String.of_seq (Seq.filter p (String.to_seq s))
(* filter string - lib - pipe seq *)
let string_pipe_filter p s = (* slower *)
  s |> String.to_seq |> (Seq.filter p) |> String.of_seq

(* split string on a list of characters - inserting back characters *)
let string_split_parse cl s =
  let len = String.length s in
  let rec split a b =
    if b = len
    then if (a = b) then [] else [String.sub s a (b - a)]
    else if List.mem s.[b] cl
    then if a = b then split (a + 1) (b + 1)
      else String.sub s a (b - a) :: Char.escaped s.[b] ::
           split (b + 1) (b + 1)
    else split a (b + 1)
  in split 0 0

(* split string on a list of characters - inserting back characters *)
let string_split cl s =
  let len = String.length s in
  let rec split a b =
    if b = len
    then if (a = b) then [] else [String.sub s a (b - a)]
    else if List.mem s.[b] cl
    then if a = b then split (a + 1) (b + 1)
      else String.sub s a (b - a) :: split (b + 1) (b + 1)
    else split a (b + 1)
  in split 0 0

(* ====================================================== *)

(* Combinations & Permutations *)

let combinations1 ?(size = 0) l =
  let rec combs = function
      [] -> [[]]
    | x::xs -> let cs = combs xs in
      List.(map (cons x) cs) @ cs
  in
  if size = 0 then combs l
  else List.(filter (fun c -> length c = size) (combs l))

let combinations ?(size = 0) l =
  let rec combs = function
      [] -> [[]]
    | x::xs -> let cs = combs xs in
      List.(map (cons x) cs) @ cs
  in
  if size = 0 then combs l
  else List.(filter (((=) size) << length) (combs l))

(* not TR *)
let permutations_td l =
  let rec interleave n x l =
  try
    let (a,b) = take_drop_e l n in
    (a @ x :: b) :: interleave (n+1) x l
  with _ -> []
  in
  let rec perms = function
    [] -> [[]]
  | x::xs ->
    List.(concat (map (interleave 0 x) (perms xs)))
  in perms l
           
(* ========================================== *)
(* fast interleave *)

let rec fast1 acc n e p =
    try let (a,b) = take_drop_e p n in
      fast1 ((a @ e :: b) :: acc) (n+1) e p
    with _ -> acc

let rec fast2 acc seen e = function
    [] -> (seen @ [e]) :: acc
  | ((x::xs) as l) ->
    fast2 ((seen @ e :: l) :: acc) (seen @ [x]) e xs

(* a bit slower *)
let permutations1 l =
  let rec interleave1 acc n e p =
    try let (a,b) = take_drop_e p n in
      interleave1 ((a @ e :: b) :: acc) (n+1) e p
    with _ -> acc
  in
  let rec perms = function
    [] -> [[]]
  | x::xs ->
    List.(concat (map (interleave1 [] 0 x) (perms xs)))
  in perms l

(* fastest *)
let permutations l =
  let rec interleave2 acc seen e = function
    [] -> (seen @ [e]) :: acc
  | ((x::xs) as l) ->
    interleave2 ((seen @ e :: l) :: acc) (seen @ [x]) e xs
  in
  let rec perms = function
    [] -> [[]]
  | x::xs ->
    List.(concat (map (interleave2 [] [] x) (perms xs)))
  in perms l

(* very slow *)
let permutations3 l =
  let rec remove e = function
    [] -> []
  | h::t -> if h = e then t else h :: remove e t
  in
  let rec perms = function
    [] -> [[]]
  | l ->
    List.concat
      (List.map
         (fun x ->
            (List.map (fun p -> x :: p) (perms (remove x l))))
            l)
  in perms l

(* slightly faster than permutations1 *)
let  permutations4 l =
  let rec interleave acc e n p =
    if n = 0 then (e :: p) :: acc
    else match take_drop n p with
        a,b -> interleave ((a @ e :: b) :: acc) e (n-1) p
  in
  let rec perms n = function
      [] -> [[]]
    | x::xs ->
      List.concat (List.map (fun p -> interleave [] x n p)
                     (perms (n-1) xs))
  in perms ((List.length l)-1) l

(* BSTs *)
(* ================================================= *)

(* BST - no value - basically removing duplicates *)
type 'a t = Lf | Br of 'a t * 'a * 'a t
                         
let rec member k = function
    Lf -> false
  | Br (_,k', _) when k' = k -> true
  | Br (l, k', r) -> if k < k' then member k l else member k r

let rec insert e = function
    Lf -> Br (Lf, e, Lf)
  | Br (l,k,r) when k = e -> Br (l,k,r)
  | Br (l,k,r) -> if e < k then Br (insert e l, k, r)
    else Br (l, k, insert e r)

let bst_of_list l =
  let rec ltos s len = function
      [] -> s
    | [x] -> insert x s
    | l ->
      begin
        let len2 = len / 2 in
        match take_drop len2 l with
          (a,x::xs) ->
          ltos (ltos (insert x s) (len2 - 1) a) (len2 - 1) xs
        | _ -> failwith "bst_of_llist"
      end
  in ltos Lf (List.length l) l

(* BST - finding histograms *)
type 'a th = Lfh | Brh of 'a th * ('a * int) * 'a th
                           
let rec geth k = function
    Lfh -> 0
  | Brh (_, (k',v), _) when k' = k -> v
  | Brh (l, (k',_), r) -> if k < k' then geth k l else geth k r

let rec inserth e = function
    Lfh -> Brh (Lfh, (e,1), Lfh)
  | Brh (l,(k,v),r) when k = e -> Brh (l,(k,v+1),r)
  | Brh (l,(k,v),r) -> if e < k then Brh (inserth e l,(k,v), r)
    else Brh (l, (k,v), inserth e r)

let hist_of_list l =
  let rec ltos s len = function
      [] -> s
    | [x] -> inserth x s
    | l ->
      begin
        let len2 = len / 2 in
        match take_drop len2 l with
          (a,x::xs) ->
          ltos (ltos (inserth x s) (len2 - 1) a) (len2 - 1) xs
        | _ -> failwith "hist_of_list"
      end
  in ltos Lfh (List.length l) l

(* BSTs with values *)
type ('a, 'b) tv = Lfv | Brv of ('a, 'b) tv * ('a * 'b) * ('a, 'b) tv
                                 
let rec getv k = function
    Lfv -> None
  | Brv (_, (k',v), _) when k' = k -> Some v
  | Brv (l, (k',_), r) -> if k < k' then getv k l else getv k r

let rec memberv k = function
    Lfv -> false
  | Brv (_,(k',_), _) when k' = k -> true
  | Brv (l, (k',_), r) -> if k < k' then memberv k l else memberv k r

let rec insertv ((e,v') as x) = function
    Lfv -> Brv (Lfv, x, Lfv)
  | Brv (l,(k,_),r) when k = e -> Brv (l,(k,v'),r)
  | Brv (l,(k,v),r) -> if e < k then Brv (insertv x l,(k,v), r)
    else Brv (l, (k,v), insertv x r)

let bstv_of_list l =
  let rec ltos s len = function
      [] -> s
    | [x] -> insertv x s
    | l ->
      begin
        let len2 = len / 2 in
        match take_drop len2 l with
          (a,x::xs) ->
          ltos (ltos (insertv x s) (len2 - 1) a) (len2 - 1) xs
        | _ -> failwith "bstv_of_list"
      end
  in ltos Lfv (List.length l) l
