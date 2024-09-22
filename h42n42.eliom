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

let%client disable_event (event : Dom_html.dragEvent Js.t Dom_html.Event.typ) (html_elt : #Dom_html.eventTarget Js.t) =
	Lwt.async (fun () ->
		Lwt_js_events.seq_loop
			(Lwt_js_events.make_event event) ~use_capture:true html_elt
			(fun ev _ -> Dom.preventDefault ev; Lwt.return ()))


let%client init_client () =

	disable_event Dom_html.Event.dragstart Dom_html.document;
	disable_event Dom_html.Event.drop Dom_html.document;

	let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
	board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
	board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
	Random.self_init ();

	let healthy_creets = ref [] in
	let quadtree = init_quadtree 4 in

	for _ = 0 to base_creet_number do
		generate_new_creet (board) (quadtree) (healthy_creets)
	done;
	
	let rec loop () =
		let%lwt () = Lwt_js.sleep base_spawn_speed in
		generate_new_creet (board) (quadtree) (healthy_creets);
		loop ()
	in
	Lwt.async loop


let%server page () =
(
	Eliom_tools.F.html
		~title:"h42n42"
		~css:[["css";"h42n42.css"]]
		Html.F.(body [
			div ~a:[a_class ["frame-background"]] [
					board_elt
				]
            ]
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

