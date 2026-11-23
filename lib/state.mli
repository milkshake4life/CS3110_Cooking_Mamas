(** Core game types and the overall game state.

    The item types ([recipe], [worker], [room]) were drafted by Colin alongside
    the shop catalog; Ethan owns this module. The game state [t] is abstract so
    that other modules can only change it through the operations below, which
    keep it valid (see the invariants on [t]).

    Note on [name]: all three item records have a field called [name]. OCaml
    picks the last-defined type ([room]) when it can't tell which one you mean,
    so annotate the type when reading [name], e.g. [fun (r : recipe) -> r.name]. *)

(** {1 Items} *)

type recipe = {
  name : string;
  ingredient_cost : int;  (** cost to make one dish *)
  price : int;  (** sale price of one dish *)
  popularity : int;  (** 1-10; higher means ordered more often *)
  level : int;  (** upgrade level, starts at 1 *)
}
(** A dish on the menu. All money is whole dollars. *)

type worker = {
  name : string;
  speed : int;  (** customers this worker can serve per day *)
  skill : int;  (** 1-10; affects dish quality *)
  wage : int;  (** paid every day *)
}
(** A worker in the kitchen. *)

type room = {
  name : string;
  seats : int;  (** extra customer capacity per day *)
  upkeep : int;  (** paid every day *)
}
(** A restaurant expansion. *)

val max_popularity : int
(** Upper bound on [popularity]. *)

val max_skill : int
(** Upper bound on [skill]. *)

(** {1 Game state} *)

type t
(** The state of one playthrough: the current day, the player's money, and
    everything the restaurant owns.

    Invariants:
    - [1 <= day t <= total_days + 1]; [day t = total_days + 1] means the game
      is over.
    - Menu recipes have unique names (compared ignoring case).
    - Rooms have unique names (compared ignoring case).
    - Workers may repeat (you can hire two Line Cooks), so they are identified
      by their position in [workers t].
    - Money only goes down through [spend] (which refuses to overspend) or
      through [apply_profit] with a negative profit. So money can become
      negative only from a losing day, never from a purchase. *)

val total_days : int
(** Number of days in a game (15). *)

type error =
  | Insufficient_funds of {
      cost : int;
      money : int;
    }  (** A purchase cost more than the player has. *)
  | Already_owned of string
      (** The named recipe or room is already on the menu / built. *)
  | Not_found of string
      (** No owned recipe has that name, or no worker is at that index. *)
  | Game_over  (** The action was attempted after the last day. *)

val error_to_string : error -> string
(** [error_to_string e] is a short message for the player describing [e]. *)

val init :
  money:int ->
  recipes:recipe list ->
  workers:worker list ->
  base_seats:int ->
  t
(** [init ~money ~recipes ~workers ~base_seats] is a new game on day 1. The
    caller supplies the starting setup (normally from [Shop]), since [State]
    can't depend on [Shop].
    Raises: [Invalid_argument] if [money < 0], [base_seats < 0], or two recipes
    share a name. *)

(** {2 Reading the state} *)

val day : t -> int
(** [day t] is the current day, starting at 1. *)

val money : t -> int
(** [money t] is the player's current money (may be negative after a losing
    day). *)

val recipes : t -> recipe list
(** [recipes t] is the menu, in the order recipes were added. *)

val workers : t -> worker list
(** [workers t] is the staff, in the order they were hired. *)

val rooms : t -> room list
(** [rooms t] is the built rooms, in the order they were built. *)

val seats : t -> int
(** [seats t] is the base seats plus the seats from every room. *)

val daily_wages : t -> int
(** [daily_wages t] is the sum of every worker's wage. *)

val daily_upkeep : t -> int
(** [daily_upkeep t] is the sum of every room's upkeep. *)

val daily_fixed_costs : t -> int
(** [daily_fixed_costs t] is [daily_wages t + daily_upkeep t]: what the
    restaurant pays each day before selling anything. *)

val has_recipe : string -> t -> bool
(** [has_recipe name t] is whether a recipe called [name] (ignoring case and
    surrounding whitespace) is on the menu. *)

val has_room : string -> t -> bool
(** [has_room name t] is whether a room called [name] (ignoring case and
    surrounding whitespace) is built. *)

val is_over : t -> bool
(** [is_over t] is whether all [total_days] days have been played. *)

(** {2 Changing the state}

    Every function returns a new state; the input is never modified. Functions
    that can fail return [Error] and leave nothing changed. All of them return
    [Error Game_over] (or, for those that can't fail, raise nothing and return
    [t] unchanged) once [is_over t]. *)

val spend : int -> t -> (t, error) result
(** [spend cost t] is [t] with [cost] subtracted from its money, or
    [Error (Insufficient_funds _)] if [money t < cost].
    Requires: [cost >= 0]. *)

val add_recipe : recipe -> t -> (t, error) result
(** [add_recipe r t] adds [r] to the end of the menu, or is
    [Error (Already_owned _)] if a recipe with the same name is already on it.
    Does not charge money; see [Shop.buy]. *)

val hire : worker -> t -> (t, error) result
(** [hire w t] adds [w] to the end of the staff. Does not charge money. *)

val add_room : room -> t -> (t, error) result
(** [add_room r t] adds [r] to the built rooms, or is [Error (Already_owned _)]
    if a room with the same name is already built. Does not charge money. *)

val update_recipe : string -> (recipe -> recipe) -> t -> (t, error) result
(** [update_recipe name f t] replaces the menu recipe called [name] with
    [f r], keeping its place on the menu, or is [Error (Not_found name)].
    Requires: [f] keeps the recipe's name. *)

val update_worker : int -> (worker -> worker) -> t -> (t, error) result
(** [update_worker i f t] replaces the [i]th worker (0-based) with [f w], or is
    [Error (Not_found _)] if there is no such worker. *)

val apply_profit : int -> t -> t
(** [apply_profit p t] adds the day's net profit [p] (negative for a loss) to
    the player's money. Intended for the simulation's end-of-day result. *)

val next_day : t -> t
(** [next_day t] moves to the following day. Once [is_over t], it returns [t]
    unchanged. *)
