open%client H42n42_types

val%client board_width : int
val%client board_height : int
val%client board_spawn_height : int
val%client board_spawn_height_start : int
val%client board_border_infection : int
val%client board_border_heal : int

val%client board_margin_left : int
val%client board_margin_top : int

val%client base_creet_number : parameters_obj

val%client base_speed : parameters_obj
val%client base_spawn_speed : float

val%client infected_life_duration : parameters_obj
val%client infected_speed_reduce_factor : float

val%client speed_increment_per_sec : float

val%client growth_delay : float

val%client base_creet_width : float
val%client base_creet_height : float
val%client hitbox_ratio : float

val%client berserker_spawn_percent : parameters_obj
val%client mean_spawn_percent : parameters_obj

val%client mean_shrink_percent : float

val%client infection_contact_percent : parameters_obj
