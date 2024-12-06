(* Advent of Code, day5 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

exception File_error

let rec split_on_null = function
    [] -> raise File_error
  | x::xs when x = "" -> [],xs
  | x::xs -> match split_on_null xs with
      (r, m) -> (x :: r, m)

let rec sort_rules rls = function
    [] -> []
  | (a,b)::t ->
    (a, List.( map snd (filter (fun (x,y) -> a=x) rls)))
    :: sort_rules rls t

let rec rev_rules rls = function
    [] -> []
  | (a,b)::t ->
    (b, List.( map fst (filter (fun (x,y) -> b=y) rls)))
    :: rev_rules rls t

let comp (a,l1) (b,l2) =
  if a > b then 1
  else if b > a then (-1) else 0

let rec check_m rls = function
    [] -> true
  | m::ms -> match getv m rls with
      None -> check_m rls ms
    | Some rbst ->
      if List.exists (fun m' -> member m' rbst) ms
      then false else check_m rls ms

let rec intersect i rl = function
    [] -> i
  | x::xs when member x rl -> intersect (i+1) rl xs
  | _::xs -> intersect i rl xs

let rearange rls pages =
  let a = Array.make (List.length pages) (-1) in
  List.iter (fun p ->
      let i = intersect 0
          (match (getv p rls) with
             None -> Lf | Some bst -> bst) (remove p pages) in
      a.(i) <- p) pages; a
  
let day5 file =
  let rawl = read_lines file in
  let (rules,manuals) = split_on_null rawl in
  let r = List.(map (fun l ->
      match l with
        [a;b] -> (int_of_string a, int_of_string b)
      | _ -> raise File_error)
      (map (String.split_on_char '|') rules)) in
  let m = List.(map (fun l -> map int_of_string l)
                  (map (String.split_on_char ',') manuals)) in
  let p_before = List.sort_uniq comp (sort_rules r r) in
  let srbst =
    List.map (fun (x,l) ->
        (x,bst_of_list (List.sort_uniq compare l))) p_before in
  let r_bst = bstv_of_list srbst in
  let (valid,wrong) = filter_split ((check_m r_bst) << List.rev) m in
  let p_after = List.sort_uniq comp (rev_rules r r) in
  let aft_bst =
    List.map (fun (x,l) ->
        (x,bst_of_list (List.sort_uniq compare l))) aft_bst in
  let bst_after = bstv_of_list aft_bst in
  let w_fixed = List.map (Array.to_list << (rearange bst_after)) wrong in
  let midr = List.map
      (fun l -> List.(hd (drop (length l / 2) l))) valid in
  let midw =  List.map
      (fun l -> List.(hd (drop (length l / 2) l))) w_fixed in  
  (List.fold_left (+) 0 midr,  List.fold_left (+) 0 midw)
