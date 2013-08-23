structure ZipperList :>
          sig
            type 'a zipperlist
            val fromList : 'a list -> 'a zipperlist
            val next : 'a zipperlist -> 'a zipperlist
            val prev : 'a zipperlist -> 'a zipperlist
            val get : 'a zipperlist -> 'a
            val set : 'a -> 'a zipperlist -> 'a zipperlist
          end
  =
struct
  type 'a zipperlist = 'a list * 'a list (* path * focus *)
  fun fromList l = (nil, l)
  fun next (_, nil) = raise Fail "nil of next"
    | next (path, h::t) = (h::path, t)
  fun prev (nil, _) = raise Fail "nil of prev"
    | prev (h::path, t) = (path, h::t)
  fun get (_, nil) = raise Fail "nil of get"
    | get (_, focus::_) = focus
  fun set _ (_, nil) = raise Fail "nil of set"
    | set newFocus (path, _ :: t) = (path, newFocus :: t)
end

structure ZipperTree :>
          sig
            datatype 'a tree = Node of 'a * 'a tree * 'a tree
                             | Leaf
            type 'a path
            type 'a zippertree

            val fromList : ('a * 'a -> bool) -> 'a list -> 'a tree
            val toZipper : 'a tree -> 'a zippertree

            val goUp : 'a zippertree -> 'a zippertree
            val goLeft : 'a zippertree -> 'a zippertree
            val goRight : 'a zippertree -> 'a zippertree
            val set : 'a -> 'a zippertree -> 'a zippertree
            val get : 'a zippertree -> 'a option
          end
  =
struct
  datatype 'a tree = Node of 'a * 'a tree * 'a tree
                   | Leaf
  datatype 'a path = Top
                   | Right of 'a * 'a tree * 'a path
                   | Left of 'a * 'a tree * 'a path
  type 'a zippertree = 'a tree * 'a path

  fun insert leq v Leaf = Node (v, Leaf, Leaf)
    | insert leq v (Node (v', left, right)) =
      if leq (v, v')
      then Node (v', insert leq v left, right)
      else Node (v', left, insert leq v right)
  fun fromList leq nil = Leaf
    | fromList leq (h::t) = insert leq h (fromList leq t)
  fun toZipper tree = (tree, Top) : 'a zippertree
  fun goUp (_, Top) = raise Fail "top of goUp"
    | goUp (tree, Right (v, left, path)) = (Node (v, left, tree), path)
    | goUp (tree, Left (v, right, path)) = (Node (v, tree, right), path)
  (* fun goDown (Leaf, _) = raise Fail "leaf of goDown" *)
  (*   | goDown (Node (left =  *)
  fun goLeft (Leaf, _) = raise Fail "leaf of goLeft"
    | goLeft (Node (v, left, right), path) = (left, Left (v, right, path))
  fun goRight (Leaf, _) = raise Fail "leaf of goLeft"
    | goRight (Node (v, left, right), path) = (right, Right (v, left, path))
  fun set v (Leaf, path) = (Node (v, Leaf, Leaf), path)
    | set v (Node (_, right, left), path) = (Node (v, right, left), path)
  fun get (Leaf, _) = NONE
    | get (Node (v, _ , _), _) = SOME v
  (* fun zinsert v (Leaf, path) = (Node (v, Leaf, Leaf), path) *)
  (*   | zinsert v (Node (v', right, left), path) = (Node (v, right, left), path) *)

end

(* test code *)
open ZipperTree
val getPrint = print o (fn s => s ^ "\n") o Int.toString o valOf o get
val tree = fromList (op <=) [4, 2, 3, 0, 1]
val ztree = toZipper tree
val () = (getPrint) ztree
val () = (getPrint o goLeft) ztree
val () = (getPrint o goUp o goLeft) ztree
val () = (getPrint o goRight o goUp o goLeft) ztree
val () = (getPrint o goRight o goRight o goUp o goLeft) ztree
val () = (getPrint o goUp o goRight o goRight o goUp o goLeft) ztree
val () = (getPrint o goLeft o goUp o goRight o goRight o goUp o goLeft) ztree
val () = (getPrint o (set 5) o goLeft o goUp o goRight o goRight o goUp o goLeft) ztree
