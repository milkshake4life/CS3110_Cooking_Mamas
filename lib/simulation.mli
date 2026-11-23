open State

type day_report = {
  day : int;
  customers_arrived : int;
  customers_served : int;
  revenue : int;
  ingredient_costs : int;
  wages : int;
  upkeep : int;
  net_profit : int;
}
(** Summary of what happened during a single simulated day *)

val simulate_day : State.t -> State.t * day_report
(** [simulate_day t] simulates one day of business using game state [t],
    updating money via [State.apply_profit], advancing to the next day, and
    returning the updated game state and a [day_report] *)
