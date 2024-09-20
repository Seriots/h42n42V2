open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

open%client H42n42_creet
open%client H42n42_params


module%server H42n42_app =
  Eliom_registration.App (
    struct
    let application_name = "h42n42"
    let global_data_path = None
end)

let%server board_elt = div ~a:[a_class ["board"]] []

let%client init_client () =
  let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
  board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
  board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
  Random.self_init ();
  
  let rec loop () =
    let%lwt () = Lwt_js.sleep base_spawn_speed in
    generate_new_creet board;
    loop ()
  in
  Lwt.async loop


let%server page () =
  (
    Eliom_tools.F.html
      ~title:"h42n42"
      ~css:[["css";"h42n42.css"]]
      Html.F.(body [
              board_elt
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

