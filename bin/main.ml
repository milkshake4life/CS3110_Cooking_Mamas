(* 15-day game loop and terminal UI. *)
open Cooking_mamas

let print_report (report : Simulation.day_report) state =
  Printf.printf
    "\n\
     Day %d\n\
     Customers served: %d of %d\n\
     Revenue: $%d\n\
     Ingredients: $%d\n\
     Wages: $%d\n\
     Upkeep: $%d\n\
     Net profit: $%d\n\
     Cash: $%d\n"
    report.day report.customers_served report.customers_arrived report.revenue
    report.ingredient_costs report.wages report.upkeep report.net_profit
    (State.money state)

let print_shop state =
  Printf.printf "\nShop before day %d | Cash: $%d\n" (State.day state)
    (State.money state);
  List.iter
    (fun (item, cost) -> Printf.printf "  $%d - %s\n" cost (Shop.describe item))
    Shop.catalog;
  print_endline "Recipe upgrades:";
  List.iter
    (fun (recipe : State.recipe) ->
      Printf.printf "  %s: $%d\n" recipe.name (Shop.upgrade_cost recipe))
    (State.recipes state);
  print_endline "Worker training:";
  List.iteri
    (fun i (worker : State.worker) ->
      Printf.printf "  %d. %s: $%d\n" (i + 1) worker.name
        (Shop.train_cost worker))
    (State.workers state)

let rec shop state =
  print_shop state;
  print_endline
    "Enter buy <item>, upgrade <recipe>, train <number>, next, or quit.";
  Printf.printf "> %!";
  match read_line () with
  | exception End_of_file -> None
  | line -> (
      let line = String.trim line in
      let command, argument =
        match String.index_opt line ' ' with
        | None -> (line, "")
        | Some i ->
            ( String.sub line 0 i,
              String.trim (String.sub line (i + 1) (String.length line - i - 1))
            )
      in
      let apply = function
        | Ok updated -> shop updated
        | Error error ->
            print_endline (State.error_to_string error);
            shop state
      in
      match String.lowercase_ascii command with
      | "next" -> Some state
      | "quit" -> None
      | "buy" -> apply (Shop.buy argument state)
      | "upgrade" -> apply (Shop.upgrade argument state)
      | "train" -> (
          match int_of_string_opt argument with
          | Some n when n > 0 -> apply (Shop.train (n - 1) state)
          | _ ->
              print_endline "Enter a positive worker number.";
              shop state)
      | _ ->
          print_endline "Unknown command.";
          shop state)

let rec play state =
  let updated, report = Simulation.simulate_day state in
  print_report report updated;
  if State.is_over updated then
    Printf.printf "\nGame over. Final cash: $%d\n" (State.money updated)
  else
    match shop updated with
    | Some next_state -> play next_state
    | None -> print_endline "Thanks for playing."

let () =
  print_endline "Cooking Mamas";
  play (Shop.new_game ())
