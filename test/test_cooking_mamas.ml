open OUnit2
open Cooking_mamas
open State
open Shop

let ramen : recipe =
  { name = "Ramen"; ingredient_cost = 4; price = 13; popularity = 7; level = 1 }

let maxed : recipe = { ramen with popularity = max_popularity; level = 4 }
let cook : worker = { name = "Cook"; speed = 10; skill = 3; wage = 25 }
let expert : worker = { cook with skill = max_skill }

(* ---------- Catalog sanity ---------- *)

let all_items =
  List.map fst catalog
  @ List.map (fun r -> Recipe r) starting_recipes
  @ List.map (fun w -> Worker w) starting_workers

let valid_item = function
  | Recipe r ->
      r.ingredient_cost > 0 && r.price > r.ingredient_cost && r.level >= 1
      && r.popularity >= 1 && r.popularity <= max_popularity
  | Worker w -> w.speed > 0 && w.wage > 0 && w.skill >= 1 && w.skill <= max_skill
  | Room r -> r.seats > 0 && r.upkeep > 0

let catalog_tests =
  [
    ( "every item has valid stats" >:: fun _ ->
      List.iter
        (fun i -> assert_bool (item_name i ^ " is invalid") (valid_item i))
        all_items );
    ( "every catalog cost is positive" >:: fun _ ->
      List.iter (fun (_, c) -> assert_bool "cost <= 0" (c > 0)) catalog );
    ( "catalog names are unique" >:: fun _ ->
      let names = List.map (fun (i, _) -> item_name i) catalog in
      assert_equal (List.length names)
        (List.length (List.sort_uniq compare names)) );
    ( "starter set matches MS2 scope" >:: fun _ ->
      let names p =
        List.sort_uniq compare
          (List.filter_map (fun i -> if p i then Some (item_name i) else None) all_items)
      in
      let recipes = List.length (names (function Recipe _ -> true | _ -> false)) in
      let workers = List.length (names (function Worker _ -> true | _ -> false)) in
      let rooms = List.length (names (function Room _ -> true | _ -> false)) in
      assert_bool "3-4 recipes" (recipes >= 3 && recipes <= 4);
      assert_bool "2-3 workers" (workers >= 2 && workers <= 3);
      assert_bool "1-2 rooms" (rooms >= 1 && rooms <= 2) );
    ( "starting setup is non-empty" >:: fun _ ->
      assert_bool "recipes" (starting_recipes <> []);
      assert_bool "workers" (starting_workers <> []);
      assert_bool "money" (starting_money > 0);
      assert_bool "seats" (base_seats > 0) );
    ( "starting day is profitable" >:: fun _ ->
      (* Rough check: if every worker serves at full speed (capped by seats)
         using the lowest-margin starter recipe, the day should still net a
         profit after wages. *)
      let served =
        min base_seats
          (List.fold_left (fun acc (w : worker) -> acc + w.speed) 0 starting_workers)
      in
      let margin =
        List.fold_left
          (fun acc (r : recipe) -> min acc (r.price - r.ingredient_cost))
          max_int starting_recipes
      in
      let wages = List.fold_left (fun acc (w : worker) -> acc + w.wage) 0 starting_workers in
      assert_bool "day 1 loses money" ((served * margin) - wages > 0) );
  ]

(* ---------- Lookup ---------- *)

let lookup_tests =
  [
    ( "find_item exact name" >:: fun _ ->
      assert_equal (Some "Ramen") (Option.map item_name (find_item "Ramen")) );
    ( "find_item ignores case and whitespace" >:: fun _ ->
      assert_equal (Some "Head Chef")
        (Option.map item_name (find_item "  head CHEF ")) );
    ("find_item unknown" >:: fun _ -> assert_equal None (find_item "Pizza"));
    ("find_item empty" >:: fun _ -> assert_equal None (find_item ""));
    ( "item_cost of catalog item" >:: fun _ ->
      assert_equal (Some 300)
        (Option.bind (find_item "Patio") item_cost) );
    ( "item_cost of unsold item" >:: fun _ ->
      assert_equal None (item_cost (Worker cook)) );
    ( "describe starts with name" >:: fun _ ->
      List.iter
        (fun (i, _) ->
          let d = describe i and n = item_name i in
          assert_equal n (String.sub d 0 (String.length n)))
        catalog );
  ]

(* ---------- Affordability ---------- *)

let afford_tests =
  [
    ("exactly enough" >:: fun _ -> assert_bool "" (can_afford 150 150));
    ("one short" >:: fun _ -> assert_bool "" (not (can_afford 149 150)));
    ("plenty" >:: fun _ -> assert_bool "" (can_afford 1000 150));
    ("zero cost" >:: fun _ -> assert_bool "" (can_afford 0 0));
  ]

(* ---------- Upgrades ---------- *)

let upgrade_tests =
  [
    ( "upgrade raises level and price" >:: fun _ ->
      let r = upgrade_recipe ramen in
      assert_equal 2 r.level ~printer:string_of_int;
      assert_bool "price" (r.price > ramen.price);
      assert_equal ramen.name r.name;
      assert_equal ramen.ingredient_cost r.ingredient_cost );
    ( "upgrade raises popularity" >:: fun _ ->
      assert_equal 8 (upgrade_recipe ramen).popularity ~printer:string_of_int );
    ( "upgrade caps popularity" >:: fun _ ->
      assert_equal max_popularity (upgrade_recipe maxed).popularity
        ~printer:string_of_int );
    ( "upgrade cost grows with level" >:: fun _ ->
      assert_bool "" (upgrade_cost (upgrade_recipe ramen) > upgrade_cost ramen) );
    ( "train raises skill, speed, wage" >:: fun _ ->
      let w = train_worker cook in
      assert_equal 4 w.skill ~printer:string_of_int;
      assert_bool "speed" (w.speed > cook.speed);
      assert_bool "wage" (w.wage > cook.wage) );
    ( "train caps skill" >:: fun _ ->
      assert_equal max_skill (train_worker expert).skill ~printer:string_of_int );
    ( "train cost grows with skill" >:: fun _ ->
      assert_bool "" (train_cost (train_worker cook) > train_cost cook) );
  ]

let () =
  run_test_tt_main
    ("cooking_mamas"
    >::: [
           "catalog" >::: catalog_tests;
           "lookup" >::: lookup_tests;
           "afford" >::: afford_tests;
           "upgrades" >::: upgrade_tests;
         ])
