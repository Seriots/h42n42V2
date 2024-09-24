open%client H42n42_types

let%client board_width = 800
let%client board_height = 800
let%client board_spawn_height = int_of_float(float(board_height) *. (36.0 /. 50.0))
let%client board_spawn_height_start = int_of_float(float(board_height) *. (8.0 /. 50.0))
let%client board_border_infection = int_of_float(float(board_height) *. (5.0 /. 50.0))
let%client board_border_heal = int_of_float(float(board_height) *. (42.0 /. 50.0))

let%client board_margin_left = 0
let%client board_margin_top = 0


let%client base_creet_number = {
  base = 10.0;
  min = 0.0;
  max = 30.0;
}

let%client base_speed = {
  base = 0.3;
  min = 0.1;
  max = 0.8;
}

let%client speed_increment_per_sec = 0.000008

let%client base_spawn_speed = 5.0

let%client infected_life_duration = {
  base = 15.0;
  min = 5.0;
  max = 30.0;  
}
let%client infected_speed_reduce_factor = 0.85

let%client growth_delay = 0.1

let%client base_creet_width = 50.0
let%client base_creet_height = 50.0
let%client hitbox_ratio = 0.85

let%client berserker_spawn_percent = {
  base = 10.0;
  min = 0.0;
  max = 45.0;
}

let%client mean_spawn_percent = {
  base = 10.0;
  min = 0.0;
  max = 45.0;
}

let%client mean_shrink_percent = 0.85

let%client infection_contact_percent = {
  base = 2.0;
  min = 0.0;
  max = 10.0;
}