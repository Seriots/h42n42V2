let%client board_width = 800
let%client board_height = 800
let%client board_spawn_height = int_of_float(float(board_height) *. (36.0 /. 50.0))
let%client board_spawn_height_start = int_of_float(float(board_height) *. (8.0 /. 50.0))
let%client board_border_infection = int_of_float(float(board_height) *. (5.0 /. 50.0))
let%client board_border_heal = int_of_float(float(board_height) *. (42.0 /. 50.0))

let%client board_margin_left = 0
let%client board_margin_top = 0

let%client base_creet_number = 5

let%client base_speed = 0.3
(* let%client speed_increment_per_sec = 0.05 *)

let%client base_spawn_speed = 2.0

let%client infected_life_duration = 12.0
let%client infected_speed_reduce_factor = 0.85

let%client growth_delay = 0.1

let%client base_creet_width = 50.0
let%client base_creet_height = 50.0
let%client hitbox_ratio = 0.85

let%client berserker_spawn_percent = 10
let%client mean_spawn_percent = 70

let%client mean_shrink_percent = 0.85


let%client infection_contact_percent = 2