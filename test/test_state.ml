(* Tests for the game state (State) and for applying shop purchases to it
   (Shop.new_game / buy / upgrade / train).

   Expected values are worked out by hand from Colin's starting setup: $100,
   Fried Rice ($2 to make, sells $8, popularity 6), one Line Cook (speed 10,
   skill 3, wage $25), and 20 base seats. *)

open OUnit2
open Cooking_mamas
open State

(* ---------- Helpers ---------- *)

let ok = function
  | Ok t -> t
  | Error e -> assert_failure ("unexpected error: " ^ error_to_string e)

let err = function
  | Ok _ -> assert_failure "expected an error"
  | Error e -> e

let pi = string_of_int
let recipe_names t = List.map (fun (r : recipe) -> r.name) (recipes t)
let worker_names t = List.map (fun (w : worker) -> w.name) (workers t)
let pl = String.concat "; "

let rice : recipe =
  { name = "Fried Rice"; ingredient_cost = 2; price = 8; popularity = 6; level = 1 }

let ramen : recipe =
  { name = "Ramen"; ingredient_cost = 4; price = 13; popularity = 7; level = 1 }

let cook : worker = { name = "Line Cook"; speed = 10; skill = 3; wage = 25 }
let chef : worker = { name = "Head Chef"; speed = 15; skill = 9; wage = 90 }
let patio : room = { name = "Patio"; seats = 10; upkeep = 15 }

let start =
  init ~money:100 ~recipes:[ rice ] ~workers:[ cook ] ~base_seats:20

(* [play_all t] is [t] after advancing through every day. *)
let rec play_all t = if is_over t then t else play_all (next_day t)

(* ---------- State: creating and reading ---------- *)

let init_tests =
  [
    ( "new game values" >:: fun _ ->
      assert_equal 1 (day start) ~printer:pi;
      assert_equal 100 (money start) ~printer:pi;
      assert_equal 20 (seats start) ~printer:pi;
      assert_equal 25 (daily_wages start) ~printer:pi;
      assert_equal 0 (daily_upkeep start) ~printer:pi;
      assert_equal 25 (daily_fixed_costs start) ~printer:pi;
      assert_equal [] (rooms start);
      assert_bool "not over" (not (is_over start)) );
    ( "init rejects negative money" >:: fun _ ->
      assert_raises (Invalid_argument "State.init: negative money") (fun () ->
          init ~money:(-1) ~recipes:[] ~workers:[] ~base_seats:0) );
    ( "init rejects duplicate recipes, ignoring case" >:: fun _ ->
      assert_raises (Invalid_argument "State.init: duplicate recipe names")
        (fun () ->
          init ~money:0
            ~recipes:[ rice; { rice with name = "fried RICE" } ]
            ~workers:[] ~base_seats:0) );
    ( "has_recipe ignores case and whitespace" >:: fun _ ->
      assert_bool "" (has_recipe "  fried rice " start);
      assert_bool "" (not (has_recipe "Ramen" start)) );
  ]

(* ---------- State: changing ---------- *)

let spend_tests =
  [
    ( "spend exactly all money" >:: fun _ ->
      assert_equal 0 (money (ok (spend 100 start))) ~printer:pi );
    ( "spend one dollar too much" >:: fun _ ->
      assert_equal
        (Insufficient_funds { cost = 101; money = 100 })
        (err (spend 101 start)) );
    ( "spend leaves the original unchanged" >:: fun _ ->
      ignore (ok (spend 60 start));
      assert_equal 100 (money start) ~printer:pi );
  ]

let item_tests =
  [
    ( "add_recipe appends to the menu" >:: fun _ ->
      assert_equal "Fried Rice; Ramen"
        (pl (recipe_names (ok (add_recipe ramen start))))
        ~printer:Fun.id );
    ( "add_recipe refuses a duplicate, ignoring case" >:: fun _ ->
      assert_equal (Already_owned "FRIED RICE")
        (err (add_recipe { rice with name = "FRIED RICE" } start)) );
    ( "hiring the same worker twice is allowed" >:: fun _ ->
      let t = ok (hire cook start) in
      assert_equal 2 (List.length (workers t)) ~printer:pi;
      assert_equal 50 (daily_wages t) ~printer:pi );
    ( "add_room adds seats and upkeep" >:: fun _ ->
      let t = ok (add_room patio start) in
      assert_equal 30 (seats t) ~printer:pi;
      assert_equal 15 (daily_upkeep t) ~printer:pi;
      assert_equal 40 (daily_fixed_costs t) ~printer:pi );
    ( "add_room refuses a duplicate" >:: fun _ ->
      let t = ok (add_room patio start) in
      assert_equal (Already_owned "Patio") (err (add_room patio t)) );
    ( "update_recipe changes only that recipe and keeps the order" >:: fun _ ->
      let t = ok (add_recipe ramen start) in
      let t = ok (update_recipe "fried rice" (fun r -> { r with price = 99 }) t) in
      assert_equal "Fried Rice; Ramen" (pl (recipe_names t)) ~printer:Fun.id;
      assert_equal [ 99; 13 ]
        (List.map (fun (r : recipe) -> r.price) (recipes t)) );
    ( "update_recipe of a missing recipe" >:: fun _ ->
      assert_equal (Not_found "Ramen") (err (update_recipe "Ramen" Fun.id start))
    );
    ( "update_worker changes only that index" >:: fun _ ->
      let t = ok (hire cook start) in
      let t = ok (update_worker 1 (fun w -> { w with wage = 0 }) t) in
      assert_equal [ 25; 0 ] (List.map (fun (w : worker) -> w.wage) (workers t))
    );
    ( "update_worker out of range" >:: fun _ ->
      assert_equal (Not_found "worker #2") (err (update_worker 1 Fun.id start));
      assert_equal (Not_found "worker #0") (err (update_worker (-1) Fun.id start))
    );
  ]

let day_tests =
  [
    ( "apply_profit can go negative" >:: fun _ ->
      assert_equal (-50) (money (apply_profit (-150) start)) ~printer:pi );
    ( "next_day advances" >:: fun _ ->
      assert_equal 2 (day (next_day start)) ~printer:pi );
    ( "day 15 is still playable" >:: fun _ ->
      let rec go n t = if n = 0 then t else go (n - 1) (next_day t) in
      let t = go (total_days - 1) start in
      assert_equal 15 (day t) ~printer:pi;
      assert_bool "not over yet" (not (is_over t)) );
    ( "game ends after the last day" >:: fun _ ->
      let t = play_all start in
      assert_equal 16 (day t) ~printer:pi;
      assert_bool "over" (is_over t) );
    ( "nothing changes after the game ends" >:: fun _ ->
      let t = play_all start in
      assert_equal 16 (day (next_day t)) ~printer:pi;
      assert_equal 100 (money (apply_profit 500 t)) ~printer:pi;
      assert_equal Game_over (err (spend 1 t));
      assert_equal Game_over (err (hire chef t)) );
  ]

(* ---------- Shop: applying purchases ---------- *)

let shop_tests =
  [
    ( "new_game uses the shop's starting setup" >:: fun _ ->
      let t = Shop.new_game () in
      assert_equal Shop.starting_money (money t) ~printer:pi;
      assert_equal Shop.base_seats (seats t) ~printer:pi;
      assert_equal 1 (day t) ~printer:pi;
      assert_equal "Fried Rice" (pl (recipe_names t)) ~printer:Fun.id;
      assert_equal "Line Cook" (pl (worker_names t)) ~printer:Fun.id );
    ( "buy a recipe" >:: fun _ ->
      (* Ramen costs $150: start with $100, earn $100, pay $150 -> $50. *)
      let t = ok (Shop.buy "  RAMEN " (apply_profit 100 start)) in
      assert_equal 50 (money t) ~printer:pi;
      assert_equal "Fried Rice; Ramen" (pl (recipe_names t)) ~printer:Fun.id );
    ( "can't afford a recipe" >:: fun _ ->
      assert_equal
        (Insufficient_funds { cost = 150; money = 100 })
        (err (Shop.buy "Ramen" start)) );
    ( "buying an owned recipe costs nothing" >:: fun _ ->
      (* Fried Rice isn't sold, so use Ramen: own it, then try again rich. *)
      let t = ok (Shop.buy "Ramen" (apply_profit 1000 start)) in
      assert_equal (Already_owned "Ramen") (err (Shop.buy "Ramen" t));
      assert_equal 950 (money t) ~printer:pi );
    ( "buy an unknown item" >:: fun _ ->
      assert_equal (Not_found "Pizza") (err (Shop.buy " Pizza " start)) );
    ( "buy a worker" >:: fun _ ->
      (* Line Cook costs $100. *)
      let t = ok (Shop.buy "line cook" start) in
      assert_equal 0 (money t) ~printer:pi;
      assert_equal "Line Cook; Line Cook" (pl (worker_names t)) ~printer:Fun.id;
      assert_equal 50 (daily_wages t) ~printer:pi );
    ( "buy a room, then refuse a second" >:: fun _ ->
      (* Patio costs $300. *)
      let t = ok (Shop.buy "Patio" (apply_profit 1000 start)) in
      assert_equal 800 (money t) ~printer:pi;
      assert_equal 30 (seats t) ~printer:pi;
      assert_equal (Already_owned "Patio") (err (Shop.buy "Patio" t)) );
    ( "upgrade a menu recipe" >:: fun _ ->
      (* Level 1 upgrade costs 75 * 1 = $75; price 8 -> 10, popularity 6 -> 7. *)
      let t = ok (Shop.upgrade "fried rice" start) in
      assert_equal 25 (money t) ~printer:pi;
      match recipes t with
      | [ r ] ->
          assert_equal 2 r.level ~printer:pi;
          assert_equal 10 r.price ~printer:pi;
          assert_equal 7 r.popularity ~printer:pi
      | _ -> assert_failure "menu should still have one recipe" );
    ( "upgrade a recipe not on the menu" >:: fun _ ->
      assert_equal (Not_found "Ramen") (err (Shop.upgrade "Ramen" start)) );
    ( "train a worker" >:: fun _ ->
      (* Skill 3 costs 40 + 20 * 3 = $100; skill 3 -> 4, speed 10 -> 11, wage
         25 -> 30. *)
      let t = ok (Shop.train 0 start) in
      assert_equal 0 (money t) ~printer:pi;
      match workers t with
      | [ w ] ->
          assert_equal 4 w.skill ~printer:pi;
          assert_equal 11 w.speed ~printer:pi;
          assert_equal 30 w.wage ~printer:pi
      | _ -> assert_failure "should still have one worker" );
    ( "train a missing worker" >:: fun _ ->
      assert_equal (Not_found "worker #2") (err (Shop.train 1 start));
      assert_equal (Not_found "worker #0") (err (Shop.train (-1) start)) );
    ( "can't afford training leaves the worker unchanged" >:: fun _ ->
      let t = ok (spend 1 start) in
      assert_equal
        (Insufficient_funds { cost = 100; money = 99 })
        (err (Shop.train 0 t));
      assert_equal 3 (List.hd (workers t)).skill ~printer:pi );
    ( "no shopping after the game ends" >:: fun _ ->
      assert_equal Game_over
        (err (Shop.buy "Line Cook" (play_all (apply_profit 1000 start)))) );
    ( "game over is reported before other errors" >:: fun _ ->
      (* Each of these would be a different error mid-game. *)
      let t = play_all (ok (Shop.buy "Patio" (apply_profit 1000 start))) in
      assert_equal Game_over (err (Shop.buy "Pizza" t));
      assert_equal Game_over (err (Shop.buy "Patio" t));
      assert_equal Game_over (err (Shop.upgrade "Ramen" t));
      assert_equal Game_over (err (Shop.train 5 t)) );
  ]

let () =
  run_test_tt_main
    ("state"
    >::: [
           "init" >::: init_tests;
           "spend" >::: spend_tests;
           "items" >::: item_tests;
           "days" >::: day_tests;
           "shop" >::: shop_tests;
         ])
