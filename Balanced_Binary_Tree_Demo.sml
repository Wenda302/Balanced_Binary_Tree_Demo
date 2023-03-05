signature iset =
sig
  type iset
  val contain : int -> iset -> bool
  val remove : int -> iset -> iset
  val insert : int -> iset -> iset

  val quick_test: bool
end;

structure list_set: iset =
struct

type iset = int list

fun contain x nil = false
  | contain x (y :: ys) 
      = (if x=y then true else contain x ys)

fun remove x nil = nil
  | remove x (y :: ys) 
    = (if x = y then remove x ys else x :: (remove x ys))

fun insert x ys = x :: ys

val quick_test = remove 4 [1,2,4] = [1,2,4]

end;

datatype tree = Leaf | Node of int * tree * tree

(*Check if a tree is binary search tree*)
fun is_bst t = 
  let 
    fun check_bound NONE _ = true
      | check_bound _ NONE = true
      | check_bound (SOME lb) (SOME ub) = lb < ub
    
    fun node_within_range Leaf lb ub = true
      | node_within_range (Node (v,lt,rt)) lb ub
        = (let val check_lb = check_bound lb (SOME v);
               val check_ub = check_bound (SOME v) ub;
               val check_lt = node_within_range lt lb (SOME v);
               val check_rt = node_within_range rt (SOME v) ub;
            in check_lb andalso check_ub andalso check_lt andalso check_rt end)
  in 
    node_within_range t NONE NONE
  end


fun get_height Leaf = 0
  | get_height (Node (v,lt,rt)) 
      = Int.max (get_height lt,get_height rt) + 1 

(*Check if a binary is balanced.*)
fun is_balanced Leaf = true
  | is_balanced (Node (v,lt,rt)) =
    (is_balanced lt andalso is_balanced rt
       andalso Int.abs (get_height lt - get_height rt) <= 1)

structure tree_set: iset =
struct


type iset = tree

fun contain _ Leaf = false
  | contain x (Node (v, lt,rt)) = 
      (if x = v then true else if x<v then contain x lt else contain x rt)

fun insert x Leaf = Node (x, Leaf,Leaf)
  | insert x (Node(v,lt, rt)) =  
      (if  x < v then Node(v, insert x lt, rt)  
      else if v < x then Node(v, lt, insert x rt)  
      else Node(v,lt, rt)) 

fun getmax (Node(v, lt, Leaf)) = (lt, v)  
  | getmax (Node(v,lt, rt)) =  
    let val (rt', m) = getmax rt  
     in (Node(v,lt, rt'), m) end  

fun join Leaf x = x  
  | join x Leaf = x  
  | join lt rt =  
    let val (l, m) = getmax lt  
     in Node(m,l, rt) end  

fun remove x Leaf = Leaf
  | remove x (Node (v, lt,rt)) = 
      (if x = v then join lt rt
        else if x<v then Node(v, (remove x lt), rt) 
        else Node(v, lt, (remove x rt)))  

val quick_test = remove 3 (Node (2, Node (1,Leaf,Leaf), Leaf)) 
            = Node (2, Node (1,Leaf,Leaf), Leaf)
end