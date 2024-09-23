open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

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

let%client game_running = ref false

let%client disable_event (event : Dom_html.dragEvent Js.t Dom_html.Event.typ) (html_elt : #Dom_html.eventTarget Js.t) =
	Lwt.async (fun () ->
		Lwt_js_events.seq_loop
			(Lwt_js_events.make_event event) ~use_capture:true html_elt
			(fun ev _ -> Dom.preventDefault ev; Lwt.return ()))


let%client start_game () =
	if not (!game_running) then (
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

		for _ = 0 to base_creet_number - 1 do
			generate_new_creet (board) (quadtree) (healthy_creets) (global_end) (start_time)
		done;


		let loop () =
			let p = 
				let rec game_loop () =
					let%lwt () = Lwt_js.sleep base_spawn_speed in
					generate_new_creet (board) (quadtree) (healthy_creets) (global_end) (start_time);
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

let%client init_client () =

	disable_event Dom_html.Event.dragstart Dom_html.document;
	disable_event Dom_html.Event.drop Dom_html.document;

	let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
	board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
	board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
	Random.self_init ()


let%server page () =
(
	Eliom_tools.F.html
		~title:"h42n42"
		~css:[["css";"h42n42.css"]]
		Html.F.(
			body ([
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

