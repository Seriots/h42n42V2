(* open%server Eliom_content *)
open%server Eliom_content.Html.D
open%server Eliom_content

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

let (>>=) = Lwt.bind
let%shared board_width = 800
let%shared board_height = 800
let%shared board_spawn_height = int_of_float(float(board_height) *. (36.0 /. 50.0))
let%shared board_spawn_height_start = int_of_float(float(board_height) *. (8.0 /. 50.0))
let%shared board_border_infection = int_of_float(float(board_height) *. (5.0 /. 50.0))

let%shared board_margin_left = 0
let%shared board_margin_top = 0

let%server board_elt = div ~a:[a_class ["board"]] []

let%client base_speed = 0.3
let%client base_spawn_speed = 0.5

let%client infected_life_duration = 8.0
let%client infected_speed_reduce_factor = 0.85

let%client growth_delay = 0.1

let%client base_creet_width = 50.0
let%client base_creet_height = 50.0

type%client state = Healthy | Infected | Berserker | Mean

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
  mutable state: state; 
  creet_elt: Dom_html.imageElement Js.t;
}

(* type%client game = {
  mutable creets: creet list;
} *)
    
let%client log (msg: string) = Js_of_ocaml.Firebug.console##log(Js.string msg)

let%client new_direction () =
  let degree = Random.float 360.0 in
  let x = (cos (degree *. 3.14159265 /. 180.)) in
  let y = (sin (degree *. 3.14159265 /. 180.)) in
  { x; y }
    
let%client set_img (dir: direction) (color: string) = 
  match dir.x > 0.0 with
  | true -> "/assets/" ^ color ^ "Right.gif"
  | false -> "/assets/" ^ color ^ "Left.gif"

let%client new_creet () = 
  let size = { width = base_creet_width; height =  base_creet_height } in
  let x = float((Random.int (board_width - int_of_float(size.width) - 32)) + 16) in
  let y = float((Random.int (board_spawn_height - int_of_float(size.height))) + board_spawn_height_start) in
  let direction = new_direction () in 
  let speed = base_speed in
  let img = set_img direction "Green" in
  let creet_elt = Dom_html.createImg Dom_html.document in
  
  creet_elt##.src := Js.string img;
  creet_elt##.alt := Js.string "CREET";
  creet_elt##.style##.position := Js.string "absolute";
  creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(x)) ^ "px");
  creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(y)) ^ "px");
  creet_elt##.style##.width := Js.string (string_of_int (int_of_float(size.width)) ^ "px");
  creet_elt##.style##.height := Js.string (string_of_int (int_of_float(size.height)) ^ "px");
  { x; y; direction; speed; img; size; creet_elt; state = Healthy }



let%client generate_new_creet board =

  let creet_obj = new_creet () in
  let endLoop = ref false in

  Dom.appendChild board creet_obj.creet_elt;
  let update_img () = 
    creet_obj.img <- set_img creet_obj.direction (match creet_obj.state with | Healthy -> "Green" | Infected -> "Yellow" | Berserker -> "Red" | Mean -> "Blue");
    creet_obj.creet_elt##.src := Js.string creet_obj.img;
  in
  let get_infected () = 
    let rd = Random.int 100 in
    if rd < 10 then
      creet_obj.state <- Berserker
    else if rd < 20 then
      creet_obj.state <- Mean
    else
      creet_obj.state <- Infected
  in
  let check_river_infection () =
    if creet_obj.y < float(board_border_infection) && creet_obj.state = Healthy then
      (
        get_infected ();
        creet_obj.speed <- creet_obj.speed *. infected_speed_reduce_factor;
        update_img ();

        let life_countdown () =
          let%lwt () = Lwt_js.sleep infected_life_duration in
          Dom.removeChild board creet_obj.creet_elt;
          endLoop := true;
          Lwt.return ()
        in
        Lwt.async (fun () -> life_countdown ());
        if creet_obj.state = Berserker then
          (
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
          )
      )
  in

  let handle_border_collision () =
    let potential_x = creet_obj.x +. creet_obj.direction.x *. creet_obj.speed in
    let potential_y = creet_obj.y +. creet_obj.direction.y *. creet_obj.speed in

    if int_of_float(potential_x) <= 0 || int_of_float(potential_x) > board_width - int_of_float(creet_obj.size.width) then
      (
        creet_obj.direction.x <- -. creet_obj.direction.x;
        update_img ()
      );
    if int_of_float(potential_y) <= 0 || int_of_float(potential_y) > board_height - int_of_float(creet_obj.size.height) then
      creet_obj.direction.y <- -. creet_obj.direction.y;
  in
  
  let move_forward () =
    creet_obj.x <- creet_obj.x +. creet_obj.direction.x *. creet_obj.speed;
    creet_obj.y <- creet_obj.y +. creet_obj.direction.y *. creet_obj.speed;
    creet_obj.creet_elt##.style##.left := Js.string (string_of_int (board_margin_left + int_of_float(creet_obj.x)) ^ "px");
    creet_obj.creet_elt##.style##.top := Js.string (string_of_int (board_margin_top + int_of_float(creet_obj.y)) ^ "px");
  in
  let random_switch_direction () = 
    match Random.int 1000 with
    | 0 -> (
            creet_obj.direction <- new_direction ();
            update_img ()
          )
    | _ -> ()
  in

  let rec update_loop () =
    let%lwt () = Lwt_js.sleep 0.0001 in
    random_switch_direction ();
    handle_border_collision ();
    move_forward ();
    check_river_infection ();
    if !endLoop then
      Lwt.return ()
    else
      update_loop ()
  in

  (* let selfkill () = 
    let%lwt () = Lwt_js.sleep 10.0 in
    Dom.removeChild board creet_obj.creet_elt;
    endLoop := true;
    Lwt.return ()
  in
  Lwt.async (fun () -> selfkill ()); *)
  Lwt.async (fun () -> update_loop ());
  
  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns creet_obj.creet_elt (fun _ _ -> 
      log "clicked";
      Lwt.return ()
    )
  )

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

