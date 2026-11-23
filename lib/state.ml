(* Types and Boundaries --> food, workers, rooms, and the overall game state *)

type recipe = {
  name : string;
  ingredient_cost : int;
  price : int;
  popularity : int;
  level : int;
}

type worker = {
  name : string;
  speed : int;
  skill : int;
  wage : int;
}

type room = {
  name : string;
  seats : int;
  upkeep : int;
}

let max_popularity = 10
let max_skill = 10

(* ---------- Game state ---------- *)

(* AF: a record [{ day; money; recipes; workers; rooms; base_seats }] is the
   game on day [day], with the player holding [money] dollars, the menu
   [recipes], staff [workers], and built [rooms] (each list in the order items
   were added), in a restaurant with [base_seats] seats before rooms.
   RI: [1 <= day <= total_days + 1]; [base_seats >= 0]; recipe names are unique
   ignoring case; room names are unique ignoring case. *)
type t = {
  day : int;
  money : int;
  recipes : recipe list;
  workers : worker list;
  rooms : room list;
  base_seats : int;
}

let total_days = 15

type error =
  | Insufficient_funds of {
      cost : int;
      money : int;
    }
  | Already_owned of string
  | Not_found of string
  | Game_over

let error_to_string = function
  | Insufficient_funds { cost; money } ->
      Printf.sprintf "Not enough money: that costs $%d and you have $%d." cost
        money
  | Already_owned name -> Printf.sprintf "You already have %s." name
  | Not_found name -> Printf.sprintf "Couldn't find %s." name
  | Game_over -> "The game is over."

let normalize s = String.lowercase_ascii (String.trim s)
let recipe_name (r : recipe) = r.name
let room_name (r : room) = r.name
let same_name a b = normalize a = normalize b

let has_duplicate_names names =
  let sorted = List.sort compare (List.map normalize names) in
  let rec go = function
    | a :: (b :: _ as rest) -> a = b || go rest
    | _ -> false
  in
  go sorted

let init ~money ~recipes ~workers ~base_seats =
  if money < 0 then invalid_arg "State.init: negative money";
  if base_seats < 0 then invalid_arg "State.init: negative base_seats";
  if has_duplicate_names (List.map recipe_name recipes) then
    invalid_arg "State.init: duplicate recipe names";
  { day = 1; money; recipes; workers; rooms = []; base_seats }

(* ---------- Reading ---------- *)

let day t = t.day
let money t = t.money
let recipes t = t.recipes
let workers t = t.workers
let rooms t = t.rooms
let sum f l = List.fold_left (fun acc x -> acc + f x) 0 l
let seats t = t.base_seats + sum (fun (r : room) -> r.seats) t.rooms
let daily_wages t = sum (fun (w : worker) -> w.wage) t.workers
let daily_upkeep t = sum (fun (r : room) -> r.upkeep) t.rooms
let daily_fixed_costs t = daily_wages t + daily_upkeep t

let has_recipe name t =
  List.exists (fun r -> same_name (recipe_name r) name) t.recipes

let has_room name t = List.exists (fun r -> same_name (room_name r) name) t.rooms
let is_over t = t.day > total_days

(* ---------- Changing ---------- *)

(* [unless_over t f] is [Error Game_over] if the game has ended, else [f ()]. *)
let unless_over t f = if is_over t then Error Game_over else f ()

let spend cost t =
  unless_over t (fun () ->
      if t.money < cost then Error (Insufficient_funds { cost; money = t.money })
      else Ok { t with money = t.money - cost })

let add_recipe (r : recipe) t =
  unless_over t (fun () ->
      if has_recipe r.name t then Error (Already_owned r.name)
      else Ok { t with recipes = t.recipes @ [ r ] })

let hire w t = unless_over t (fun () -> Ok { t with workers = t.workers @ [ w ] })

let add_room (r : room) t =
  unless_over t (fun () ->
      if has_room r.name t then Error (Already_owned r.name)
      else Ok { t with rooms = t.rooms @ [ r ] })

let update_recipe name f t =
  unless_over t (fun () ->
      if not (has_recipe name t) then Error (Not_found name)
      else
        Ok
          {
            t with
            recipes =
              List.map
                (fun r -> if same_name (recipe_name r) name then f r else r)
                t.recipes;
          })

let update_worker i f t =
  unless_over t (fun () ->
      if i < 0 || i >= List.length t.workers then
        Error (Not_found (Printf.sprintf "worker #%d" (i + 1)))
      else
        Ok
          {
            t with
            workers = List.mapi (fun j w -> if j = i then f w else w) t.workers;
          })

let apply_profit p t = if is_over t then t else { t with money = t.money + p }
let next_day t = if is_over t then t else { t with day = t.day + 1 }
