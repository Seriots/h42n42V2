open%server Eliom_content
open%server Eliom_content.Html.D

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt


module%server H42n42_app =
  Eliom_registration.App (
    struct
      let application_name = "h42n42"
      let global_data_path = None
    end)

let%server width  = 700
let%server height = 400

let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke

let%server canvas_elt =
  Html.D.canvas ~a:[Html.D.a_width width; Html.D.a_height height]
    [Html.D.txt "your browser doesn't support canvas"]

let%server page () =
  html
     (head (title (txt "h42n42")) [])
     (body [h1 [txt "h42n42"];
            canvas_elt])


let%client init_client () =
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";

  let x = ref 0 and y = ref 0 in

  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0; y := ev##.clientY - y0
  in

  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    ((0, 0, 0), 5, (oldx, oldy), (!x, !y))
  in

  let line ev = draw ctx (compute_line ev); Lwt.return () in

  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns canvas
      (fun ev _ ->
          set_coord ev;
          let%lwt () = line ev in
          Lwt.pick
            [mousemoves Dom_html.document (fun x _ -> line x);
            let%lwt ev = mouseup Dom_html.document in line ev]))

let%server main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server () =
  H42n42_app.register
    ~service:main_service
    (fun () () ->
       (* Cf. section "Client side side-effects on the server" *)
       let _ = [%client (init_client () : unit) ] in
       Lwt.return (page ()))