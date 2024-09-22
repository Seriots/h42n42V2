open%client H42n42_types

val%client quadtree_size : quadtree -> int
val%client log_quad_tree : quadtree -> unit

val%client quadtree_remove : quadtree -> creet -> unit
val%client quadtree_insert : quadtree -> creet -> unit
val%client quadtree_check_collisions : quadtree -> creet -> bool

val%client init_quadtree : int -> quadtree