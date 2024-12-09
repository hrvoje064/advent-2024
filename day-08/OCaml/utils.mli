(* File reading tools - utilities *)

(* Interface file - utils.mli *)

val time : (unit -> 'a) -> 'a

val time_opt : (unit -> 'a * 'b) -> string * 'a * 'b 
  
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

(* checking if string is a number - integer or float *)
val is_number : string -> bool

(* checking if string is integer *)
val is_int : string -> bool

(* checking if string is float *)
val is_float : string -> bool

(* remove first found element from a list *)
val remove : 'a -> 'a list -> 'a list

(* remove all found elements from a list *)
val remove_all : 'a -> 'a list -> 'a list

(* returns a pair of lists - left true, right false elements *)
val filter_split : ('a -> bool) -> 'a list -> 'a list * 'a list

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
val string_split_parse : char list -> string -> string list

(* split string on a list of characters *)
val string_split : char list -> string -> string list

(* ====================================================== *)

(* Combinations & Permutations *)
val combinations : ?size:int -> 'a list -> 'a list list

(* Permutations *)
val permutations : 'a list -> 'a list list    

(* ====================================================== *)

(* faster version with BST - finding duplicates *)

(* BST - no values, just finding elements fast *)
type 'a t = Lf | Br of 'a t * 'a * 'a t

(* finding if e is member of BST *)
val member : 'a -> 'a t -> bool

(* Making BST from a list *)
val bst_of_list : 'a list -> 'a t

(* finding number of specific element in a list - histogram *)
type 'a th = Lfh | Brh of 'a th * ('a * int) * 'a th

(* get number of repeated elemnets *)
val geth : 'a -> 'a th -> int

(* make BST histogram from list of of elements *)
val hist_of_list : 'a list -> 'a th

(* BSTs with values *)
type ('a, 'b) tv = Lfv | Brv of ('a, 'b) tv * ('a * 'b) * ('a, 'b) tv

val getv : 'a -> ('a, 'b) tv -> 'b option

val memberv : 'a -> ('a, 'b) tv -> bool

val bstv_of_list : ('a * 'b) list -> ('a, 'b) tv
