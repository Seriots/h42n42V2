(* open%server Eliom_content *)
open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

let (>>=) = Lwt.bind
let%shared board_width = 200
let%shared board_height = 200

let%shared board_margin_left = 100
let%shared board_margin_top = 100

let%server board_elt = div ~a:[a_class ["board"]] []

type direction = {
  mutable x : int;
  mutable y : int;
}
  
type creet = {
  mutable x : int;
  mutable y : int;
  mutable direction: direction;
  mutable speed: int;
  mutable img: string;
}

type game = {
  mutable creets: creet list;
}
    
    
let%client new_creet x y = 
  let creet = Dom_html.createImg Dom_html.document in
  creet##.src := Js.string "/assets/ghost.png";
  creet##.alt := Js.string "CREET";
  creet##.style##.position := Js.string "absolute";
  creet##.style##.left := Js.string (string_of_int (board_margin_left + x) ^ "px");
  creet##.style##.top := Js.string (string_of_int (board_margin_top + y) ^ "px");
  creet


let%client generate_new_creet board =
  let x = Random.int board_width in
  let y = Random.int board_height in
  let creet = new_creet x y in
  Dom.appendChild board creet;
  
  let rec update_loop () =
    let%lwt () = Lwt_js.sleep 0.1 in
    let x = x + Random.int 3 - 1 in
    let y = y + Random.int 3 - 1 in
    creet##.style##.left := Js.string (string_of_int (board_margin_left + x) ^ "px");
    creet##.style##.top := Js.string (string_of_int (board_margin_top + y) ^ "px");
    update_loop ()
  in
  Lwt.async (fun () -> update_loop ());
  
  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns creet (fun _ _ -> 
      Js_of_ocaml.Firebug.console##log (Js.string ("yolooooo" ^ string_of_int(x) ^ " " ^ string_of_int(y)));
      Lwt.return ()
    )
  )

      
let%client init_client () =
  let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
  board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
  board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
  board##.style##.marginLeft := Js.string (string_of_int(board_margin_left) ^ "px");
  board##.style##.marginTop := Js.string (string_of_int(board_margin_top) ^ "px");
  
  Random.self_init ();
  
  let rec loop () =
    let%lwt () = Lwt_js.sleep 5.0 in
    generate_new_creet board;
    loop ()
  in
  Lwt.async loop


module%server H42n42_app =
  Eliom_registration.App (
    struct
    let application_name = "h42n42"
    let global_data_path = None
end)



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
    (fun () () ->
      let _ = [%client (init_client () : unit) ] in
    Lwt.return (page ()))

