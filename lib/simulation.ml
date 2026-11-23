(* Day simulation & economy engine *)
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

let total_worker_speed (workers : worker list) : int =
  List.fold_left (fun acc (w : worker) -> acc + w.speed) 0 workers

let average_skill (workers : worker list) : int =
  match workers with
  | [] -> 0
  | _ ->
      let sum =
        List.fold_left (fun acc (w : worker) -> acc + w.skill) 0 workers
      in
      sum / List.length workers

let pick_dish (recipes : recipe list) (index : int) : recipe option =
  match recipes with
  | [] -> None
  | _ ->
      let total_pop =
        List.fold_left
          (fun acc (r : recipe) -> acc + max 1 r.popularity)
          0 recipes
      in
      let target = index mod total_pop in
      let rec find remaining = function
        | [] -> None
        | [ last ] -> Some last
        | r :: rest ->
            let pop = max 1 r.popularity in
            if remaining < pop then Some r else find (remaining - pop) rest
      in
      find target recipes

let simulate_day (t : State.t) : State.t * day_report =
  let day = State.day t in
  let capacity = State.seats t in
  let recipes = State.recipes t in
  let workers = State.workers t in

  let max_service = total_worker_speed workers in

  let menu_pop =
    List.fold_left (fun acc (r : recipe) -> acc + r.popularity) 0 recipes
  in
  let raw_demand = menu_pop * 2 in
  let customers_arrived = max 0 (min capacity raw_demand) in

  let customers_served =
    if recipes = [] then 0 else min customers_arrived max_service
  in

  let rec serve_orders n rev cost =
    if n >= customers_served then (rev, cost)
    else
      match pick_dish recipes n with
      | None -> (rev, cost)
      | Some dish ->
          serve_orders (n + 1) (rev + dish.price) (cost + dish.ingredient_cost)
  in
  let revenue, ingredient_costs = serve_orders 0 0 0 in

  let wages = State.daily_wages t in
  let upkeep = State.daily_upkeep t in
  let net_profit = revenue - ingredient_costs - wages - upkeep in

  let t_with_profit = State.apply_profit net_profit t in
  let t_final = State.next_day t_with_profit in

  let report =
    {
      day;
      customers_arrived;
      customers_served;
      revenue;
      ingredient_costs;
      wages;
      upkeep;
      net_profit;
    }
  in
  (t_final, report)
