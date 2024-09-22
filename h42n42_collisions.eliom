open%client H42n42_types
open%client H42n42_params


let%client log_quad_tree (quadtree: quadtree) : unit =
	match quadtree.root with
	| Some node -> 
		let rec log_node (node: quadtreeNode) : unit =
			match node with
			| node ->
				match node.childNo, node.childNe, node.childSo, node.childSe with
				| None, None, None, None -> ()
				| Some no, Some ne, Some so, Some se -> 
					log_node no;
					log_node ne;
					log_node so;
					log_node se
				| _ -> ()
		in
		log_node node
	| None -> ()

let%client quadtree_size (quadtree: quadtree) : int =
	let rec size (node: quadtreeNode) : int =
		match node with
		| node ->
			match node.childNo, node.childNe, node.childSo, node.childSe with
			| None, None, None, None -> List.length node.contains
			| Some no, Some ne, Some so, Some se -> 
				size no + size ne + size so + size se
			| _ -> 0
	in
	match quadtree.root with
	| Some node -> size node
	| None -> 0


let%client quadtree_remove (quadtree: quadtree) (creet: creet) : unit =
	match quadtree.root with
	| Some node -> 
		let rec remove_creet (node: quadtreeNode) (creet: creet) : unit =
			match node with
			| node ->
				match node.childNo, node.childNe, node.childSo, node.childSe with
				| None, None, None, None -> (node.contains <- List.filter (fun c -> c != creet) node.contains);
				| Some no, Some ne, Some so, Some se -> 
					remove_creet no creet;
					remove_creet ne creet;
					remove_creet so creet;
					remove_creet se creet
				| _ -> ()
		in
		remove_creet node creet
	| None -> ()


(*
	Check if a creet fits in a node
	@param node: a quadtree node
	@param creet: a creet
	@return: a boolean
*)
let%client fit_creet_in_node (node: quadtreeNode) (creet: creet) : bool =
	let creet_width_adjusted = creet.size.width *. hitbox_ratio in
	let creet_height_adjusted = creet.size.height *. hitbox_ratio in
	match node with
	| node ->
			creet.x >= float_of_int node.startX && creet.x <= float_of_int (node.startX + node.width)
				&& creet.y >= float_of_int node.startY && creet.y <= float_of_int (node.startY + node.height)
		||	creet.x +. creet_width_adjusted >= float_of_int node.startX && creet.x +. creet_width_adjusted <= float_of_int (node.startX + node.width)
				&& creet.y >= float_of_int node.startY && creet.y <= float_of_int (node.startY + node.height)
		||	creet.x >= float_of_int node.startX && creet.x <= float_of_int (node.startX + node.width)
				&& creet.y +. creet_height_adjusted >= float_of_int node.startY && creet.y +. creet_height_adjusted <= float_of_int (node.startY + node.height)
		||	creet.x +. creet_width_adjusted >= float_of_int node.startX && creet.x +. creet_width_adjusted <= float_of_int (node.startX + node.width)
				&& creet.y +. creet_height_adjusted >= float_of_int node.startY && creet.y +. creet_height_adjusted <= float_of_int (node.startY + node.height)


let%client rec insert_creet (node: quadtreeNode) (creet: creet) : unit =
	match node with
	| node ->
		if fit_creet_in_node (node) (creet) then
			
			match node.childNo, node.childNe, node.childSo, node.childSe with
			| None, None, None, None -> (node.contains <- creet::node.contains)
			| Some no, Some ne, Some so, Some se -> 
				insert_creet se creet;
				insert_creet no creet;
				insert_creet ne creet;
				insert_creet so creet
			| _ -> ()
			

let%client quadtree_insert (quadtree: quadtree) (creet: creet) : unit =
	match quadtree.root with
	| Some node -> insert_creet node creet
	| None -> ()


let%client is_colliding (creet1: creet) (creet2: creet) : bool =
	let centerX1 = creet1.x +. creet1.size.width /. 2. in
	let centerY1 = creet1.y +. creet1.size.height /. 2. in
	let centerX2 = creet2.x +. creet2.size.width /. 2. in
	let centerY2 = creet2.y +. creet2.size.height /. 2. in
	abs_float(centerX1 -. centerX2) < (creet1.size.width *. hitbox_ratio +. creet2.size.width *. hitbox_ratio) /. 2. &&
	abs_float(centerY1 -. centerY2) < (creet1.size.height *. hitbox_ratio +. creet2.size.height *. hitbox_ratio) /. 2.

let%client quadtree_check_collisions (quadtree: quadtree) (creet: creet) : bool = 
	match quadtree.root with
	| Some node -> 
		let rec check_collisions (node: quadtreeNode) (creet: creet) : bool =
			match node with
			| node ->
				if fit_creet_in_node (node) (creet) then
					match node.childNo, node.childNe, node.childSo, node.childSe with
					| None, None, None, None -> (List.exists (fun c -> is_colliding c creet) node.contains)
					| Some no, Some ne, Some so, Some se -> 
							check_collisions no creet
						||	check_collisions ne creet
						||	check_collisions so creet
						||	check_collisions se creet
					| _ -> false
				else false
		in
		check_collisions node creet
	| None -> false


(*
	Initialize a quad tree node
	@param x: x coordinate of the node
	@param y: y coordinate of the node
	@param width: width of the node
	@param height: height of the node
	@param maxDepth: maximum depth of the quad tree
	@return: a quadtreenode option
*)
let%client rec init_quadtree_node (x:int) (y:int) (width:int) (height:int) (maxDepth: int) : quadtreeNode option =
	match maxDepth with
	| 0 -> None
	| _ -> Some {
		contains=[];
		childNo=init_quadtree_node (x) (y) (width / 2) (height / 2) (maxDepth-1);
		childNe=init_quadtree_node (x + width / 2) (y) (width / 2) (height / 2) (maxDepth-1);
		childSo=init_quadtree_node (x) (y + height / 2) (width / 2) (height / 2) (maxDepth-1);
		childSe=init_quadtree_node (x + width / 2) (y + height / 2) (width / 2) (height / 2) (maxDepth-1);
		startX=x;
		startY=y;
		width=width;
		height=height
    }


(*
	Initialize a quad tree
	@param maxDepth: maximum depth of the quad tree
	@return: a quadtree
*)
let%client init_quadtree (maxDepth:int) : quadtree =
	let root = init_quadtree_node (0) (0) (board_width) (board_height) (maxDepth) in
	{root=root; maxDepth=maxDepth}