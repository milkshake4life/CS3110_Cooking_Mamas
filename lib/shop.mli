(** The shop catalog: every recipe, worker, and room in the game, plus the
    starting setup and the helpers the shop menu uses. *)

type item =
  | Recipe of State.recipe
  | Worker of State.worker
  | Room of State.room  (** Something the player can buy between days. *)

(** {1 Starting setup} *)

val starting_money : int
(** Money the player has on day 1. *)

val starting_recipes : State.recipe list
(** Recipes on the menu on day 1. *)

val starting_workers : State.worker list
(** Workers hired on day 1. *)

val base_seats : int
(** Seats in the restaurant before any room upgrades. *)

(** {1 Catalog} *)

val catalog : (item * int) list
(** Every purchasable item paired with its purchase cost. Names are unique. *)

val item_name : item -> string
(** [item_name i] is the display name of [i]. *)

val item_cost : item -> int option
(** [item_cost i] is the purchase cost of [i], or [None] if [i] is not
    sold in the shop. *)

val find_item : string -> item option
(** [find_item name] is the catalog item called [name] (ignoring case and
    surrounding whitespace), or [None] if there is none. *)

val can_afford : int -> int -> bool
(** [can_afford money cost] is whether [money] covers [cost]. *)

val describe : item -> string
(** [describe i] is a one-line summary of [i]'s stats for the shop menu. *)

(** {1 Upgrades} *)

val upgrade_cost : State.recipe -> int
(** [upgrade_cost r] is the cost to raise [r] to the next level. It grows
    with each level. *)

val upgrade_recipe : State.recipe -> State.recipe
(** [upgrade_recipe r] is [r] one level higher, with a higher price and
    popularity (popularity capped at [State.max_popularity]). *)

val train_cost : State.worker -> int
(** [train_cost w] is the cost to train [w] once. *)

val train_worker : State.worker -> State.worker
(** [train_worker w] is [w] after training: more skill (capped at
    [State.max_skill]), slightly faster, and a higher wage. *)
