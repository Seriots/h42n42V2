open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

open%client H42n42_types
open%client H42n42_params
open%client H42n42_collisions
open%client H42n42_creet


module%server H42n42_app =
  Eliom_registration.App (
    struct
    let application_name = "h42n42"
    let global_data_path = None
end)

let%server board_elt = div ~a:[a_class ["board"]] []
let%server time_elt = div ~a:[a_class ["stats-time"]] [txt "Time 00:00"]
let%server healthyCount_elt = div ~a:[a_class ["stats-healthy"]] [txt "Healthy 0"]

let%server game_over_elt = div ~a:[a_class ["game-over-board"]] [div ~a:[a_class ["game-over"]] [txt "Game Over"]]

let%server base_creet_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("34.0"); ] ()
let%server base_speed_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("29.0") ] ()
let%server mean_percent_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("22.23") ] ()
let%server berserker_percent_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("22.23") ] ()
let%server infecton_contact_percent_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("20.0") ] ()
let%server infected_life_duration_elt = input ~a:[a_input_type `Range; a_class ["slide-bar"]; a_value ("40.0") ] ()

let%client game_running = ref false


(*
	Disable basic drag and drop event
	@param event: a Dom_html.dragEvent Js.t Dom_html.Event.typ
	@param html_elt: a Dom_html.eventTarget Js.t
	@return: unit
*)
let%client disable_event (event : Dom_html.dragEvent Js.t Dom_html.Event.typ) (html_elt : #Dom_html.eventTarget Js.t) =
	Lwt.async (fun () ->
		Lwt_js_events.seq_loop
			(Lwt_js_events.make_event event) ~use_capture:true html_elt
			(fun ev _ -> Dom.preventDefault ev; Lwt.return ()))


(*
	Wrapper to handle the parameters
	@param percent: a float
	@param params: a parameters_obj
	@return: a float
*)
let%client get_params_value (percent: float) (params: parameters_obj) : float =
	percent *. (params.max -. params.min) /. 100. +. params.min


(*
	Called on button start onclick, start the game
	@return: unit
*)
let%client start_game () =
	if not (!game_running) then (
		let s_base_creet = Eliom_content.Html.To_dom.of_input ~%base_creet_elt in
		let s_base_speed = Eliom_content.Html.To_dom.of_input ~%base_speed_elt in
		let s_mean_percent = Eliom_content.Html.To_dom.of_input ~%mean_percent_elt in
		let s_berserker_percent = Eliom_content.Html.To_dom.of_input ~%berserker_percent_elt in
		let s_infecton_contact_percent = Eliom_content.Html.To_dom.of_input ~%infecton_contact_percent_elt in
		let s_infected_life_duration = Eliom_content.Html.To_dom.of_input ~%infected_life_duration_elt in
		let time_stats = Eliom_content.Html.To_dom.of_div ~%time_elt in
		let healthy_stats = Eliom_content.Html.To_dom.of_div ~%healthyCount_elt in
		let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
		
		let game_over = Eliom_content.Html.To_dom.of_div ~%game_over_elt in
		game_over##.style##.display := Js.string "none";

		board##.innerHTML := Js.string "";
		let healthy_creets = ref [] in
		let quadtree = init_quadtree 4 in
		let global_end = ref false in
		let healthy_counter = ref 0 in
		let start_time = (new%js Js.date_now)##getTime in
		let timer = ref 0.0 in
		let all_params = {
			base_speed = get_params_value (s_base_speed##.value |> Js.to_string |> float_of_string) (base_speed);
			base_creet_number = get_params_value (s_base_creet##.value |> Js.to_string |> float_of_string) (base_creet_number);
			mean_percent = get_params_value (s_mean_percent##.value |> Js.to_string |> float_of_string) (mean_spawn_percent);
			berserker_percent = get_params_value (s_berserker_percent##.value |> Js.to_string |> float_of_string) (berserker_spawn_percent);
			infection_contact_percent = get_params_value (s_infecton_contact_percent##.value |> Js.to_string |> float_of_string) (infection_contact_percent);
			infected_life_duration = infected_life_duration.max -. get_params_value (s_infected_life_duration##.value |> Js.to_string |> float_of_string) (infected_life_duration);
		} in

		for _ = 0 to int_of_float(all_params.base_creet_number) - 1 do
			generate_new_creet (board) (quadtree) (healthy_creets) (global_end) (start_time) (all_params)
		done;


		let loop () =
			let p = 
				let rec game_loop () =
					let%lwt () = Lwt_js.sleep base_spawn_speed in
					generate_new_creet (board) (quadtree) (healthy_creets) (global_end) (start_time) (all_params);
					game_loop ()
				in
				game_loop ()
			in
			let rec check_end_game () =
				let%lwt () = Lwt_js.sleep 0.1 in
				healthy_counter := List.length !healthy_creets;
				timer := (new%js Js.date_now)##getTime -. start_time;
				time_stats##.innerHTML := Js.string (Printf.sprintf "Time %02d:%02d" (int_of_float(!timer /. 1000.0) / 60) (int_of_float (!timer /. 1000.0) mod 60));
				healthy_stats##.innerHTML := Js.string ("Healthy " ^ (string_of_int !healthy_counter));
				if !healthy_counter > 0 then
					check_end_game ()
				else (
					Lwt.cancel p;
					global_end := true;
					game_running := false;
					game_over##.style##.display := Js.string "block";
					Lwt.return ()
				)
			in
			check_end_game ()
		in
		game_running := true;
		Lwt.async (fun () -> loop ())
	)


(*
	Basic initialization of the client
*)
let%client init_client () =

	disable_event Dom_html.Event.dragstart Dom_html.document;
	disable_event Dom_html.Event.drop Dom_html.document;

	let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
	board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
	board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
	Random.self_init ()


(*
		The page serv to the client
*)
let%server page () =
(
	Eliom_tools.F.html
		~title:"h42n42"
		~css:[["css";"h42n42.css"]]
		Html.F.(
			body ([
				div ~a: [a_class ["left-side"]] [
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Start Creet"];
						base_creet_elt;
					];
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Basic Speed"];
						base_speed_elt;
					];
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Virus Power"];
						infected_life_duration_elt;
					];
				];
				div ~a: [a_class ["right-side"]] [
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Nasty Mean %"];
						mean_percent_elt;
					];
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Berserker %"];
						berserker_percent_elt;
					];
					div ~a:[a_class ["slide-bar-background"]] [
						div ~a:[a_class ["slide-bar-text"]] [txt "Virus Spread"];
						infecton_contact_percent_elt;
					];
				];
				div ~a:[a_class ["frame-background"]] [
					game_over_elt;
					board_elt
				];
				div ~a:[a_class ["bottom-side"]] [
					time_elt;
					button ~a:[a_class ["start-button"]; a_onclick
						[%client fun _ -> start_game () ]
					] [];
					healthyCount_elt;
				];

			])
		)
)
        
let%server main_service =
	Eliom_service.create
		~path:(Eliom_service.Path [])
		~meth:(Eliom_service.Get Eliom_parameter.unit)
	()


let%server () =
	H42n42_app.register
		~service:main_service
		(
			fun () () ->
			let _ = [%client (init_client () : unit) ] in
			Lwt.return (page ())
		)

