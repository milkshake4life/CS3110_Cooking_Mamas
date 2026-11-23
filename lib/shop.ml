(* Shop catalog & upgrade logic --> restaurant, expansion rooms, chefs, etc. *)

open State

type item =
  | Recipe of recipe
  | Worker of worker
  | Room of room

(* ---------- Recipes ---------- *)

let fried_rice : recipe =
  { name = "Fried Rice"; ingredient_cost = 2; price = 8; popularity = 6; level = 1 }

let ramen : recipe =
  { name = "Ramen"; ingredient_cost = 4; price = 13; popularity = 7; level = 1 }

let omurice : recipe =
  { name = "Omurice"; ingredient_cost = 4; price = 12; popularity = 8; level = 1 }

let wagyu_steak : recipe =
  { name = "Wagyu Steak"; ingredient_cost = 14; price = 38; popularity = 5; level = 1 }

(* ---------- Workers ---------- *)

let line_cook : worker = { name = "Line Cook"; speed = 10; skill = 3; wage = 25 }
let sous_chef : worker = { name = "Sous Chef"; speed = 12; skill = 6; wage = 50 }
let head_chef : worker = { name = "Head Chef"; speed = 15; skill = 9; wage = 90 }

(* ---------- Rooms ---------- *)

let patio : room = { name = "Patio"; seats = 10; upkeep = 15 }
let second_location : room = { name = "Second Location"; seats = 30; upkeep = 60 }

(* ---------- Starting setup ---------- *)

let starting_money = 100
let starting_recipes = [ fried_rice ]
let starting_workers = [ line_cook ]
let base_seats = 20

(* ---------- Catalog ---------- *)

let catalog =
  [
    (Recipe ramen, 150);
    (Recipe omurice, 200);
    (Recipe wagyu_steak, 500);
    (Worker line_cook, 100);
    (Worker sous_chef, 250);
    (Worker head_chef, 600);
    (Room patio, 300);
    (Room second_location, 1500);
  ]

let item_name = function
  | Recipe r -> r.name
  | Worker w -> w.name
  | Room r -> r.name

let normalize s = String.lowercase_ascii (String.trim s)

let item_cost i =
  List.find_map
    (fun (i', cost) -> if item_name i' = item_name i then Some cost else None)
    catalog

let find_item name =
  let key = normalize name in
  List.find_map
    (fun (i, _) -> if normalize (item_name i) = key then Some i else None)
    catalog

let can_afford money cost = money >= cost

let describe = function
  | Recipe r ->
      Printf.sprintf "%s (Lv %d): sells $%d, costs $%d to make, popularity %d/10"
        r.name r.level r.price r.ingredient_cost r.popularity
  | Worker w ->
      Printf.sprintf "%s: speed %d, skill %d/10, wage $%d/day" w.name w.speed
        w.skill w.wage
  | Room r -> Printf.sprintf "%s: +%d seats, upkeep $%d/day" r.name r.seats r.upkeep

(* ---------- Upgrades ---------- *)

let upgrade_cost (r : recipe) = 75 * r.level

let upgrade_recipe (r : recipe) =
  {
    r with
    level = r.level + 1;
    price = r.price + 2;
    popularity = min max_popularity (r.popularity + 1);
  }

let train_cost (w : worker) = 40 + (20 * w.skill)

let train_worker (w : worker) =
  { w with skill = min max_skill (w.skill + 1); speed = w.speed + 1; wage = w.wage + 5 }
