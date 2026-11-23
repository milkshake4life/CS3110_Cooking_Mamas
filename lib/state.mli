(** Core game types.

    PROPOSAL: these item types were drafted alongside the shop catalog so it
    could compile and be tested. Ethan owns this module and may rename or
    extend anything here; [Shop] only relies on the fields below. *)

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
