open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

open%client H42n42_params


type%client e_state = Healthy | Infected | Berserker | Mean

type%client direction = {
	mutable x : float;
	mutable y : float;
}

type%client creetSize = {
	mutable width : float;
	mutable height : float;
}
  
type%client creet = {
	mutable x : float;
	mutable y : float;
	mutable img: string;
	mutable direction: direction;
	mutable speed: float;
	size: creetSize;
	mutable state: e_state; 
	creet_elt: Dom_html.imageElement Js.t;
}

(*
	Wrapper for Js console log
	@param msg: string -> the msg to log
	@return unit
*)
let%client log (msg: string) : unit = Js_of_ocaml.Firebug.console##log(Js.string msg)


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
let%client new_creet () = 
	let size = { width = base_creet_width; height =  base_creet_height } in
	let x = float((Random.int (board_width - int_of_float(size.width) - 32)) + 16) in
	let y = float((Random.int (board_spawn_height - int_of_float(size.height))) + board_spawn_height_start) in
	let direction = new_direction () in 
	let speed = base_speed in
	let state = Healthy in
	let img = get_new_img direction state in
	let creet_elt = Dom_html.createImg Dom_html.document in
  
	creet_elt##.src := Js.string img;
	creet_elt##.alt := Js.string "CREET";
	creet_elt##.className := Js.string "creet";
	creet_elt##.style##.position := Js.string "absolute";
	creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(x)) ^ "px");
	creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(y)) ^ "px");
	creet_elt##.style##.width := Js.string (string_of_int (int_of_float(size.width)) ^ "px");
	creet_elt##.style##.height := Js.string (string_of_int (int_of_float(size.height)) ^ "px");
	{ x; y; direction; speed; img; size; creet_elt; state }


(*
	Apply the basic infection effect on the creet
		- reduce the speed of the creet
		- remove the creet from the board after a certain time
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
	@return unit
*)
let%client basic_infection (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) = 
	creet_obj.speed <- creet_obj.speed *. infected_speed_reduce_factor;
	let life_countdown () =
		let%lwt () = Lwt_js.sleep infected_life_duration in
		Dom.removeChild board creet_obj.creet_elt;
		endLoop := true;
		Lwt.return ()
	in	
	Lwt.async (fun () -> life_countdown ());
	update_img (creet_obj)


(*
	Call the basic infection and apply the specific effect of the berserker infection
		- increase the size of the creet
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
	@return unit
*)
let%client berserker_infection (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) = 
	basic_infection (creet_obj) (board) (endLoop);
	let rec berserker_growth () = 
		let%lwt () = Lwt_js.sleep growth_delay in
		creet_obj.size.width <- creet_obj.size.width +. (base_creet_width *. (1.0 /. (infected_life_duration /. growth_delay)));
		creet_obj.size.height <- creet_obj.size.height +. (base_creet_height *. (1.0 /. (infected_life_duration /. growth_delay)));
		log(string_of_float creet_obj.size.width);
		creet_obj.creet_elt##.style##.width := Js.string (string_of_int (int_of_float(creet_obj.size.width)) ^ "px");
		creet_obj.creet_elt##.style##.height := Js.string (string_of_int (int_of_float(creet_obj.size.height)) ^ "px");
		if !endLoop then
			Lwt.return ()
		else
			berserker_growth ()
	in
	Lwt.async (fun () -> berserker_growth ())


(*
	Infected a creet based on a random number
		- 10% chance to spawn a berserker
		- 10% chance to spawn a mean
		- 80% chance to spawn a basic infected
	@param creet_obj: creet -> the creet object to infect
	@param board: Dom_html.divElement Js.t -> the board where the creet is
	@param endLoop: bool ref -> the reference to the loop of the creet
*)
let%client get_infected (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) = 

	let rd = Random.int 100 in
	if rd < berserker_spawn_percent then
	(
		creet_obj.state <- Berserker;
		berserker_infection (creet_obj) (board) (endLoop)
	)
	else if rd < berserker_spawn_percent + mean_spawn_percent then
	(
		creet_obj.state <- Mean;
		basic_infection (creet_obj) (board) (endLoop)
	)
	else
	(
		creet_obj.state <- Infected;
		basic_infection (creet_obj) (board) (endLoop)
	)


(*
	Check if the creet is in the river and is healthy
	@param creet_obj: creet -> the creet object to check
	@return bool
*)
let%client check_river_infection (creet_obj: creet) : bool =
	if creet_obj.y < float(board_border_infection) && creet_obj.state = Healthy then
		true
	else
		false


(*
	Check if the creet is in the river and not infected, if it's the case, infect the creet 
	@param creet_obj: creet -> the creet object to check
	@return bool
*)
let%client check_infection (creet_obj: creet) (board: Dom_html.divElement Js.t) (endLoop: bool ref) =
	if check_river_infection(creet_obj) then
		get_infected (creet_obj) (board) (endLoop)


(*
	Check if the creet is colliding with the border of the board and change the direction of the creet
	@param creet_obj: creet -> the creet object to check
	@return unit
*)
let%client handle_border_collision (creet_obj: creet) =
	let potential_x = creet_obj.x +. creet_obj.direction.x *. creet_obj.speed in
	let potential_y = creet_obj.y +. creet_obj.direction.y *. creet_obj.speed in

	if int_of_float(potential_x) <= 0 || int_of_float(potential_x) > board_width - int_of_float(creet_obj.size.width) then
	(
		creet_obj.direction.x <- -. creet_obj.direction.x;
		update_img (creet_obj)
	);
	if int_of_float(potential_y) <= 0 || int_of_float(potential_y) > board_height - int_of_float(creet_obj.size.height) then
		creet_obj.direction.y <- -. creet_obj.direction.y


(*
	Move the creet forward based on its direction and speed
	@param creet_obj: creet -> the creet object to move
	@return unit
*)
let%client move_forward (creet_obj: creet) =
	creet_obj.x <- creet_obj.x +. creet_obj.direction.x *. creet_obj.speed;
	creet_obj.y <- creet_obj.y +. creet_obj.direction.y *. creet_obj.speed;
	creet_obj.creet_elt##.style##.left := Js.string (string_of_int (int_of_float(creet_obj.x)) ^ "px");
	creet_obj.creet_elt##.style##.top := Js.string (string_of_int (int_of_float(creet_obj.y)) ^ "px")


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
	Generate a new creet and start its loop
	@param board: Dom_html.divElement Js.t -> the board where the creet will be
	@return unit
*)
let%client generate_new_creet board =

	let creet_obj = new_creet () in
	let endLoop = ref false in
	let is_selected = ref false in

	Dom.appendChild board creet_obj.creet_elt;

	let rec update_loop () =
		let%lwt () = Lwt_js.sleep 0.0001 in
		if not (!is_selected) then
		(
			random_switch_direction (creet_obj);
			handle_border_collision (creet_obj);
			move_forward (creet_obj);
			check_infection (creet_obj) (board) (endLoop);
		);
		if !endLoop then
			Lwt.return ()
		else
			update_loop ()
	in
	Lwt.async (fun () -> update_loop ());

	
	Lwt.async (fun () ->
		let open Lwt_js_events in
		mousedowns creet_obj.creet_elt (fun _ _ -> 
			is_selected := true;
	        let _  = 
         		Lwt.pick
           		[mousemoves Dom_html.document (fun _ _ -> log("move"); Lwt.return ());
            	mouseups Dom_html.document (fun _ _ ->  is_selected := false; Lwt.return())] in
			(* Lwt.pick [
				(
					let%lwt () = Lwt_js_events.mousemoves Dom_html.document (fun ev _ ->
						let mouse_x = float_of_int ev##.clientX in
						let mouse_y = float_of_int ev##.clientY in
						creet_obj.x <- mouse_x;
						creet_obj.y <- mouse_y;
						creet_obj.creet_elt##.style##.left := Js.string (string_of_int (int_of_float(creet_obj.x)) ^ "px");
						creet_obj.creet_elt##.style##.top := Js.string (string_of_int (int_of_float(creet_obj.y)) ^ "px");
						Lwt.return ()
					) in
					Lwt.return ()
				);
				(
					let%lwt () = Lwt_js_events.mouseups Dom_html.document (fun _ _ -> 
						is_selected := false;
						Lwt.return ()
					) in
					Lwt.return ()
				)
			] in *)
			Lwt.return ()

		)
	)