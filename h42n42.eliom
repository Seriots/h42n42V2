(* open%server Eliom_content *)
open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

let (>>=) = Lwt.bind
let%shared board_width = 800
let%shared board_height = 600

let%shared board_margin_left = 0
let%shared board_margin_top = 0

let%server board_elt = div ~a:[a_class ["board"]] []

let%client base_speed = 0.3
let%client base_spawn_speed = 0.5

let%client base_creet_width = 40
let%client base_creet_height = 40


type%client direction = {
  mutable x : float;
  mutable y : float;
}

type%client creetSize = {
  width : int;
  height : int;
}
  
type%client creet = {
  mutable x : float;
  mutable y : float;
  mutable img: string;
  direction: direction;
  speed: float;
  size: creetSize;
  creet_elt: Dom_html.imageElement Js.t;
}

(* type%client game = {
  mutable creets: creet list;
} *)
    
let%client log msg = Js_of_ocaml.Firebug.console##log(Js.string msg)

let%client new_direction () =
  let degree = Random.float 360.0 in
  let x = (cos (degree *. 3.14159265 /. 180.)) in
  let y = (sin (degree *. 3.14159265 /. 180.)) in
  Js_of_ocaml.Firebug.console##log (Js.string ("new dir" ^ string_of_float(x) ^ " " ^ string_of_float(y)));
  { x; y }
    
let%client new_creet () = 
  let size = { width = base_creet_width; height =  base_creet_height } in
  let x = float(Random.int (board_width - size.width)) in
  let y = float(Random.int (board_height - size.height)) in
  let direction = new_direction () in 
  let speed = base_speed in
  let img = "/assets/ghost.png" in
  let creet_elt = Dom_html.createImg Dom_html.document in
  
  creet_elt##.src := Js.string img;
  creet_elt##.alt := Js.string "CREET";
  creet_elt##.style##.position := Js.string "absolute";
  creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(x)) ^ "px");
  creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(y)) ^ "px");
  creet_elt##.style##.width := Js.string (string_of_int (size.width) ^ "px");
  creet_elt##.style##.height := Js.string (string_of_int (size.height) ^ "px");
  { x; y; direction; speed; img; size; creet_elt; }



let%client generate_new_creet board =

  let creet_obj = new_creet () in

  Dom.appendChild board creet_obj.creet_elt;

  let handle_border_collision () =
    let potential_x = creet_obj.x +. creet_obj.direction.x *. creet_obj.speed in
    let potential_y = creet_obj.y +. creet_obj.direction.y *. creet_obj.speed in

    if int_of_float(potential_x) <= 0 || int_of_float(potential_x) > board_width - creet_obj.size.width then
      creet_obj.direction.x <- -. creet_obj.direction.x;
      log (string_of_float potential_x);
    if int_of_float(potential_y) <= 0 || int_of_float(potential_y) > board_height - creet_obj.size.height then
      creet_obj.direction.y <- -. creet_obj.direction.y;
  in
  
  let move_forward () =
    creet_obj.x <- creet_obj.x +. creet_obj.direction.x *. creet_obj.speed;
    creet_obj.y <- creet_obj.y +. creet_obj.direction.y *. creet_obj.speed;
    creet_obj.img <- creet_obj.img;
    creet_obj.creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(creet_obj.x)) ^ "px");
    creet_obj.creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(creet_obj.y)) ^ "px");
  in
  let rec update_loop () =
    let%lwt () = Lwt_js.sleep 0.0001 in
    handle_border_collision ();
    move_forward ();
    update_loop ()
  in
  Lwt.async (fun () -> update_loop ());
  
  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns creet_obj.creet_elt (fun _ _ -> 
      Lwt.return ()
    )
  )

      
let%client init_client () =
  let board = Eliom_content.Html.To_dom.of_div ~%board_elt in
  board##.style##.width := Js.string (string_of_int(board_width) ^ "px");
  board##.style##.height := Js.string (string_of_int(board_height) ^ "px");
  (* board##.style##.marginLeft := Js.string (string_of_int(board_margin_left) ^ "px"); *)
  (* board##.style##.marginTop := Js.string (string_of_int(board_margin_top) ^ "px"); *)
  
  Random.self_init ();
  
  let rec loop () =
    let%lwt () = Lwt_js.sleep base_spawn_speed in
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

