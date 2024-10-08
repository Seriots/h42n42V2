open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

open%client H42n42_types
open%client H42n42_params
open%client H42n42_collisions


(*
	Wrapper for Js console log
	@param msg: string -> the msg to log
	@return unit
*)
(* let%client log (msg: string) : unit = Js_of_ocaml.Firebug.console##log(Js.string msg) *)


(*
	Generates a random direction object
	@return direction
*)
let%client new_direction () =
	let degree = Random.float 360.0 in
	let x = (cos (degree *. 3.14159265 /. 180.)) in
	let y = (sin (degree *. 3.14159265 /. 180.)) in
	{ x; y }
    

(*
	Return the image path of the creet based on its direction and state
	@param dir: direction -> the direction of the creet
	@param state: e_state -> the current state of the creet
	@return string
*)
let%client get_new_img (dir: direction) (state: e_state) =
	let color = match state with | Healthy -> "Green" | Infected -> "Yellow" | Berserker -> "Red" | Mean -> "Blue" in
	match dir.x > 0.0 with
	| true -> "/assets/" ^ color ^ "Right.gif"
	| false -> "/assets/" ^ color ^ "Left.gif"


(*
	Update the image of the creet based on its direction and state and modifie it in the dom object
	@param creet_obj: creet -> the creet object to update
	@return unit
*)
let%client update_img (creet_obj: creet) =
	creet_obj.img <- get_new_img creet_obj.direction creet_obj.state;
	creet_obj.creet_elt##.src := Js.string creet_obj.img


(*
	Move the creet forward based on its direction and speed
	@param creet_obj: creet -> the creet object to move
	@return unit
*)
let%client move_forward (creet_obj: creet) =
	let speed = creet_obj.speed +. ((new%js Js.date_now)##getTime -. creet_obj.start_time) *. speed_increment_per_sec in
	creet_obj.x <- creet_obj.x +. creet_obj.direction.x *. speed;
	creet_obj.y <- creet_obj.y +. creet_obj.direction.y *. speed;
	creet_obj.creet_elt##.style##.left := Js.string (string_of_int (int_of_float(creet_obj.x)) ^ "px");
	creet_obj.creet_elt##.style##.top := Js.string (string_of_int (int_of_float(creet_obj.y)) ^ "px")



(*
	Move the creet forward based on its direction and speed
	@param creet_obj: creet -> the creet object to move
	@return unit
*)
let%client move_to_target (creet_obj: creet) =
	let dir = match creet_obj.target with
		| Some target -> 
			let x = target.x -. creet_obj.x in
			let y = target.y -. creet_obj.y in
			let norm = sqrt (x *. x +. y *. y) in
			{ x = x /. norm; y = y /. norm }
		| None -> creet_obj.direction
	in
	if (creet_obj.direction.x *. dir.x) < 0.0 then
	(
		creet_obj.direction <- dir;
		update_img (creet_obj);
	)
	else
		creet_obj.direction <- dir;
	move_forward (creet_obj)

(*
	Initialize a new creet object
		- set the size of the creet
		- set the position of the creet
		- set the direction of the creet
		- set the speed of the creet
		- set the state of the creet
		- set the image of the creet
		- Create a dom object for the creet
	@return creet
*)
let%client new_creet (start_time: float) (params: all_params) = 
	let size = { width = base_creet_width; height =  base_creet_height } in
	let x = float((Random.int (board_width - int_of_float(size.width) - 32)) + 16) in
	let y = float((Random.int (board_spawn_height - int_of_float(size.height))) + board_spawn_height_start) in
	let direction = new_direction () in 
	let speed = params.base_speed in
	let state = Healthy in
	let img = get_new_img direction state in
	let id = Random.int 1000000 in
	let target = None in
	let move_fonction = move_forward in
	let creet_elt = Dom_html.createImg Dom_html.document in
  
	creet_elt##.src := Js.string img;
	creet_elt##.alt := Js.string "CREET";
	creet_elt##.className := Js.string "creet";
	creet_elt##.style##.position := Js.string "absolute";
	creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(x)) ^ "px");
	creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(y)) ^ "px");
	creet_elt##.style##.width := Js.string (string_of_int (int_of_float(size.width)) ^ "px");
	creet_elt##.style##.height := Js.string (string_of_int (int_of_float(size.height)) ^ "px");
	{ x; y; direction; speed; img; size; state; target; move_fonction; start_time; creet_elt; id }


(*
	Apply the basic infection effect on the creet
		- reduce the speed of the creet
		- remove the creet from the board after a certain time
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
	@return unit
*)
let%client basic_infection (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) (isSelected: bool ref) (isHealed: bool ref) (quadtree: quadtree) (healthy_creets: creet list ref) (infected_duration: float) = 
	creet_obj.speed <- creet_obj.speed *. infected_speed_reduce_factor;
	isHealed := false;
	update_img creet_obj;
	let life_countdown () =
		let p = 
			let%lwt () = Lwt_js.sleep infected_duration in
			let rec waiting_selection () =
				let%lwt () = Lwt_js.sleep 0.1 in
				if not (!isSelected) then
				(
					if not (creet_obj.state = Healthy) then
					(
						Dom.removeChild (board) (creet_obj.creet_elt);
						quadtree_remove (quadtree) (creet_obj);
						endLoop := true;
					);
					Lwt.return ()
				)
				else
					waiting_selection ()
			in
			waiting_selection ()
		in
		let rec check_heal () =
			let%lwt () = Lwt_js.sleep 0.1 in
			if !isHealed then
			(
				Lwt.cancel p;
				quadtree_remove (quadtree) (creet_obj);
				healthy_creets := creet_obj :: !healthy_creets;
				Lwt.return ()
			)
			else
				check_heal ()
		in
		check_heal ()
	in
	Lwt.async (fun () -> life_countdown ())


(*
	If healed restore the size of a creet
	@param creet_obj: creet -> the creet object to restore
	@return unit
*)
let%client restore_size (creet_obj: creet) = 
	creet_obj.size.width <- base_creet_width;
	creet_obj.size.height <- base_creet_height;
	creet_obj.creet_elt##.style##.width := Js.string (string_of_int (int_of_float(creet_obj.size.width)) ^ "px");
	creet_obj.creet_elt##.style##.height := Js.string (string_of_int (int_of_float(creet_obj.size.height)) ^ "px")


(*
	Apply the specific effect of the berserker infection
		- increase the size of the creet
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
	@return unit
*)
let%client berserker_infection (creet_obj: creet) (endLoop: bool ref) (isHealed: bool ref) (infected_duration: float) = 
	let rec berserker_growth () = 
		let%lwt () = Lwt_js.sleep growth_delay in
		if !isHealed then
		(
			restore_size (creet_obj);
			Lwt.return ()
		)
		else
		(
			creet_obj.size.width <- creet_obj.size.width +. (base_creet_width *. (1.0 /. (infected_duration /. growth_delay)));
			creet_obj.size.height <- creet_obj.size.height +. (base_creet_height *. (1.0 /. (infected_duration /. growth_delay)));
			creet_obj.creet_elt##.style##.width := Js.string (string_of_int (int_of_float(creet_obj.size.width)) ^ "px");
			creet_obj.creet_elt##.style##.height := Js.string (string_of_int (int_of_float(creet_obj.size.height)) ^ "px");
			if !endLoop then
				Lwt.return ()
			else
				berserker_growth ()
		);

	in
	Lwt.async (fun () -> berserker_growth ())


(*
	Apply the specific effect of the mean infection
		- reduce the size of the creet
		- change the target of the creet to a healthy creet
	@param creet_obj: creet -> the creet object to infect
	@param endLoop: bool ref -> the reference to the loop of the creet
	@return unit
*)
let%client mean_infection (creet_obj: creet) (endLoop: bool ref) (isHealed: bool ref) (healthy_creets: creet list ref) = 
	creet_obj.size.width <- base_creet_width *. mean_shrink_percent;
	creet_obj.size.height <- base_creet_height *. mean_shrink_percent;
	creet_obj.creet_elt##.style##.width := Js.string (string_of_int (int_of_float(creet_obj.size.width)) ^ "px");
	creet_obj.creet_elt##.style##.height := Js.string (string_of_int (int_of_float(creet_obj.size.height)) ^ "px");
	creet_obj.move_fonction <- move_to_target;
	let rec mean_target_reset () = 
		let%lwt () = Lwt_js.sleep 0.1 in
		if !isHealed then
		(
			restore_size (creet_obj);
			creet_obj.target <- None;
			creet_obj.move_fonction <- move_forward;
			Lwt.return ()
		)
		else
		(
			if match creet_obj.target with
				| None -> true
				| Some target -> target.state != Healthy
			then
			(
				let new_target = List.fold_left (fun acc x -> if x.state = Healthy then Some x else acc) None !healthy_creets in
				creet_obj.target <- new_target
			);
			
			if !endLoop then
				Lwt.return ()
			else
				mean_target_reset ()
		);

	in
	Lwt.async (fun () -> mean_target_reset ())

(*
	Infected a creet based on a random number
		- 10% chance to spawn a berserker
		- 10% chance to spawn a mean
		- 80% chance to spawn a basic infected
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
*)
let%client get_infected (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) (isSelected: bool ref) (isHealed: bool ref) (quadtree: quadtree) (healthy_creets: creet list ref) (params: all_params) = 

	let rd = Random.int 100 in
	healthy_creets := List.filter (fun x -> x != creet_obj) !healthy_creets;
	if rd < int_of_float(params.berserker_percent) then
	(
		creet_obj.state <- Berserker;
		basic_infection (creet_obj) (board) (endLoop) (isSelected) (isHealed) (quadtree) (healthy_creets) (params.infected_life_duration);
		berserker_infection (creet_obj) (endLoop) (isHealed) (params.infected_life_duration)
	)
	else if rd < int_of_float(params.berserker_percent +. params.mean_percent) then
	(
		creet_obj.state <- Mean;
		basic_infection (creet_obj) (board) (endLoop) (isSelected) (isHealed) (quadtree) (healthy_creets) (params.infected_life_duration);
		mean_infection (creet_obj) (endLoop) (isHealed) (healthy_creets)
	)
	else
	(
		creet_obj.state <- Infected;
		basic_infection (creet_obj) (board) (endLoop) (isSelected) (isHealed) (quadtree) (healthy_creets) (params.infected_life_duration)
	)


(*
	Check if the creet is in the river and is healthy
	@param creet_obj: creet -> the creet object to check
	@return bool
*)
let%client check_river_infection (creet_obj: creet) : bool =
	if creet_obj.y < float(board_border_infection) then
		true
	else
		false


(*
	Infect creet if it collide a infected and creet with a certain probability
	@param quadtree: quadtree -> the quadtree containing all the creets
	@param creet: creet -> the creet to check
	@param infection_percent: float -> the probability of infection
	@return bool
*)
let%client wrapper_quadtree_collisions (quadtree: quadtree) (creet: creet) (infection_percent: float) : bool = 
	let is_collide = quadtree_check_collisions (quadtree) (creet) in
	if is_collide && (Random.int 100) < int_of_float(infection_percent) then
		true
	else
		false

(*
	Check if the creet is in the river and not infected, if it's the case, infect the creet 
	@param creet_obj: creet -> the creet object to check
	@return bool
*)
let%client check_infection (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) (isSelected: bool ref) (isHealed: bool ref)  (quadtree: quadtree) (healthy_creets: creet list ref) (params: all_params) =
	match creet_obj.state with
		| Healthy -> (
			if check_river_infection (creet_obj) || wrapper_quadtree_collisions (quadtree) (creet_obj) (params.infection_contact_percent)  then
				get_infected (creet_obj) (board) (endLoop) (isSelected) (isHealed) (quadtree) (healthy_creets) (params);
		)
		| _ -> ()

(*
	Check if the creet is colliding with the border of the board and change the direction of the creet
	@param creet_obj: creet -> the creet object to check
	@return unit
*)
let%client handle_border_collision (creet_obj: creet) =
	let potential_x = creet_obj.x +. creet_obj.direction.x *. creet_obj.speed in
	let potential_y = creet_obj.y +. creet_obj.direction.y *. creet_obj.speed in

	if int_of_float(potential_x) <= 0 || (int_of_float(potential_x) > (board_width - int_of_float(creet_obj.size.width)) && creet_obj.direction.x > 0.) then
	(
		creet_obj.direction.x <- -. creet_obj.direction.x;
		update_img (creet_obj)
	);
	if int_of_float(potential_y) <= 0 || (int_of_float(potential_y) > (board_height - int_of_float(creet_obj.size.height)) && creet_obj.direction.y > 0.) then
		creet_obj.direction.y <- -. creet_obj.direction.y


(*
	Randomly switch the direction of the creet
	@param creet_obj: creet -> the creet object to switch
	@return unit
*)
let%client random_switch_direction (creet_obj: creet) = 
	match Random.int 1000 with
	| 0 -> (
			creet_obj.direction <- new_direction ();
			update_img (creet_obj)
			)
	| _ -> ()


(*
	Compute the mouse coord and apply it to the grabed creet
	@param creet_obj: creet -> the creet object to move
	@param ev: Dom_html.mouseEvent Js.t -> the mouse event
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@return unit
*)
let%client set_coord (creet_obj: creet) (ev: Dom_html.mouseEvent Js.t) (board: Dom_html.divElement Js.t) =
	let x0, y0 = Dom_html.elementClientPosition board in
	creet_obj.x <- float (max (min (ev##.clientX - x0 - int_of_float(creet_obj.size.width) / 2) (board_width - (int_of_float(creet_obj.size.width) + 8))) 8);
	creet_obj.y <- float (max (min (ev##.clientY - y0 - int_of_float(creet_obj.size.height) / 2) (board_height - (int_of_float(creet_obj.size.height) + 8))) 8)


(*
	Apply the mouse coord to the creet and update its position in the dom object
	@param creet_obj: creet -> the creet object to move
	@param ev: Dom_html.mouseEvent Js.t -> the mouse event
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@return unit
*)
let%client drag_creet (creet_obj: creet) (ev: Dom_html.mouseEvent Js.t) (board: Dom_html.divElement Js.t)=
	set_coord creet_obj ev board;
	creet_obj.creet_elt##.style##.left := Js.string (string_of_int (int_of_float(creet_obj.x)) ^ "px");
	creet_obj.creet_elt##.style##.top := Js.string (string_of_int (int_of_float(creet_obj.y)) ^ "px");
	Lwt.return ()


(*
	Check if the creet is in the heal zone and heal it
	@param creet_obj: creet -> the creet object to heal
	@param isHealed: bool ref -> the reference to the heal state of the creet
	@return unit
*)
let%client drop_creet (creet_obj: creet) (isHealed: bool ref) (endLoop: bool ref) =
	if !endLoop then (Lwt.return ())
	else (
		if creet_obj.y > float(board_border_heal) then
		(
			creet_obj.state <- Healthy;
			update_img creet_obj;
			isHealed := true
		);	
		Lwt.return ()
	)

(*
	Update the quadtree
	@param quadtree: a quad tree
	@param creet: a creet
	@return unit
*)
let%client modify_quadtree (quadtree: quadtree) (creet_obj: creet) = 
	match creet_obj.state with
	| Healthy -> ();
	| _ -> (
		quadtree_remove (quadtree) (creet_obj);
		quadtree_insert (quadtree) (creet_obj)
	)

(*
	Generate a new creet and start its loop
	@param board: Dom_html.divElement Js.t -> the board where the creet will be
	@return unit
*)
let%client generate_new_creet (board: Dom_html.divElement Js.t) (quadtree: quadtree) (healthy_creets: creet list ref) (global_end: bool ref) (start_time: float) (params: all_params) =

	let creet_obj = new_creet (start_time) (params) in
	let endLoop = ref false in
	let isSelected = ref false in
	let isHealed = ref false in

	Dom.appendChild (board) (creet_obj.creet_elt);
	healthy_creets := creet_obj :: !healthy_creets;

	let rec update_loop () =
		let%lwt () = Lwt_js.sleep 0.0005 in
		if not (!isSelected) then
		(
			random_switch_direction (creet_obj);
			handle_border_collision (creet_obj);
			creet_obj.move_fonction (creet_obj);
			modify_quadtree (quadtree) (creet_obj);
			check_infection (creet_obj) (board) (endLoop) (isSelected) (isHealed) (quadtree) (healthy_creets) (params);
		);
		if !global_end then (
			endLoop := true;
		);
		if !endLoop then (
			quadtree_remove (quadtree) (creet_obj);
			Lwt.return ()
		)
		else
			update_loop ()
	in
	Lwt.async (fun () -> update_loop ());

	let rec check_end_loop () =
		let%lwt () = Lwt_js.sleep 0.1 in
		if !endLoop then
			Lwt.return ()
		else
			check_end_loop ()
	in

	Lwt.async (fun () ->
		let open Lwt_js_events in
		mousedowns creet_obj.creet_elt (fun ev _ ->
			if !endLoop then
				Lwt.return ()
			else if creet_obj.state = Healthy && List.length !healthy_creets <= 1 then
				Lwt.return ()
			else
			(
				isSelected := true;
				healthy_creets := List.filter (fun x -> x != creet_obj) !healthy_creets;
				quadtree_remove (quadtree) (creet_obj);
				let%lwt () = (drag_creet (creet_obj) (ev) (board)) in
				Lwt.pick
					[
						mousemoves Dom_html.document (fun x _ -> (drag_creet (creet_obj) (x) (board)));
						check_end_loop ();
						let%lwt _ = mouseup Dom_html.document in (
																	isSelected := false;
																	(match creet_obj.state with | Healthy -> (healthy_creets := creet_obj :: !healthy_creets) | _ -> ());
																	drop_creet (creet_obj) (isHealed) (endLoop)
																)
					]
			)
		)
	)
