(* File reading tools - utilities *)

(* Interface file - utils.mli *)

val time : (unit -> 'a) -> 'a
  
val time_void : (unit -> 'a) -> unit
  
val take_drop : int -> 'a list -> 'a list * 'a list

val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list
                                    
val take_drop_e : 'a list -> int -> 'a list * 'a list
                                      
val take_e : 'a list -> int -> 'a list

val drop_e : 'a list -> int -> 'a list

(* compose *)
val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b  

(* string to list of chars *)
val explode : string -> char list

(*list to string *)
val implode : char list -> string    

(* interval from -- to *)
val ( -- ) : int -> int -> int list

(* checking if string is a number - integer *)
val is_number : string -> bool

(* ============================================ *)
(* File Tools *)
(* =============================================== *)
    
(* read the entire file as a string *)
val read_file : string -> string
  
(* read lines - returns list of strings *)
val read_lines : string -> string list

(* read and process line by line - my generic function *)
val read_process_lines : (string -> 'a) -> string -> 'a list

(* read and process line by line - my generic function
   copy of Scheme function read-lol-words:
   read a file into list of lists of strings
   splits every string (from read-lines) into a list of
   word strings split on space - extra spaces removed *)
val read_lol_words : string -> string list list

(* filter string - my - much faster than converting to seq
   bl = buffer length *)
val string_filter_b : int -> (char -> bool) -> string -> string

(* split string on a list of characters - but inserting back the
   actual character as a string. Used for parsing
   string_split_parse "abcmul(123,456).done" ['(';',';')'] ==>
   ["abcmul"; "("; "123"; ","; "456"; ")"; ".done"] *)
val string_split_parse : string -> char list -> string list

(* ====================================================== *)

(* Combinations & Permutations *)
val combinations : ?size:int -> 'a list -> 'a list list

(* Permutations *)
val permutations : 'a list -> 'a list list    

(* ====================================================== *)

(* faster version with BST - finding duplicates *)
(* finding number of specific element in a list *)
type 'a t = Lf | Br of 'a t * ('a * 'a) * 'a t

(* get number of repeated elemnets *)
val get : int -> int t -> int

(* make BST from list of ints *)
val bst_of_list : int list -> int t
    
