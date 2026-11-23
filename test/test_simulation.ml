open OUnit2
open Cooking_mamas
open State
open Shop
open Simulation

(* ---------- Helpers ---------- *)

let pi = string_of_int

let ok = function
  | Ok t -> t
  | Error e -> assert_failure ("unexpected error: " ^ error_to_string e)

let rice : recipe =
  {
    name = "Fried Rice";
    ingredient_cost = 2;
    price = 8;
    popularity = 6;
    level = 1;
  }

let ramen : recipe =
  { name = "Ramen"; ingredient_cost = 4; price = 13; popularity = 7; level = 1 }

let cook : worker = { name = "Line Cook"; speed = 10; skill = 3; wage = 25 }
let chef : worker = { name = "Head Chef"; speed = 15; skill = 9; wage = 90 }
let patio : room = { name = "Patio"; seats = 10; upkeep = 15 }

(* ---------- Basic & Edge Case Simulation ---------- *)

let basic_tests =
  [
    ( "starting game day 1 matches intended math" >:: fun _ ->
      let start = Shop.new_game () in
      let t, report = simulate_day start in
      (* menu_pop = 6 -> raw_demand = 12 -> arrived = 12 -> served = 10 (cook
         speed) revenue = 10 * 8 = 80; ingredients = 10 * 2 = 20; wages = 25;
         upkeep = 0 net_profit = 80 - 20 - 25 = 35 -> money = 135, day = 2 *)
      assert_equal 1 report.day ~printer:pi;
      assert_equal 12 report.customers_arrived ~printer:pi;
      assert_equal 10 report.customers_served ~printer:pi;
      assert_equal 80 report.revenue ~printer:pi;
      assert_equal 20 report.ingredient_costs ~printer:pi;
      assert_equal 25 report.wages ~printer:pi;
      assert_equal 0 report.upkeep ~printer:pi;
      assert_equal 35 report.net_profit ~printer:pi;
      assert_equal 135 (money t) ~printer:pi;
      assert_equal 2 (day t) ~printer:pi );
    ( "empty menu serves zero customers and loses fixed costs" >:: fun _ ->
      let st = init ~money:50 ~recipes:[] ~workers:[ cook ] ~base_seats:20 in
      let t, report = simulate_day st in
      assert_equal 0 report.customers_arrived ~printer:pi;
      assert_equal 0 report.customers_served ~printer:pi;
      assert_equal 0 report.revenue ~printer:pi;
      assert_equal 0 report.ingredient_costs ~printer:pi;
      assert_equal 25 report.wages ~printer:pi;
      assert_equal (-25) report.net_profit ~printer:pi;
      assert_equal 25 (money t) ~printer:pi );
    ( "zero workers serves zero customers even with high demand" >:: fun _ ->
      let st = init ~money:50 ~recipes:[ rice ] ~workers:[] ~base_seats:20 in
      let t, report = simulate_day st in
      assert_equal 12 report.customers_arrived ~printer:pi;
      assert_equal 0 report.customers_served ~printer:pi;
      assert_equal 0 report.revenue ~printer:pi;
      assert_equal 0 report.net_profit ~printer:pi;
      assert_equal 50 (money t) ~printer:pi );
  ]

(* ---------- Capacity, Speed & Multi-Dish Tests ---------- *)

let scaling_tests =
  [
    ( "seating capacity caps customer arrivals" >:: fun _ ->
      (* Fried Rice (pop 6) + Ramen (pop 7) = 13 pop -> raw_demand = 26, but
         base_seats = 20 caps arrivals at 20 *)
      let st =
        init ~money:100 ~recipes:[ rice; ramen ] ~workers:[ cook; chef ]
          ~base_seats:20
      in
      let _, report = simulate_day st in
      assert_equal 20 report.customers_arrived ~printer:pi;
      assert_equal 20 report.customers_served ~printer:pi );
    ( "rooms increase seating capacity and deduct daily upkeep" >:: fun _ ->
      let st =
        init ~money:500 ~recipes:[ rice; ramen ] ~workers:[ cook; chef ]
          ~base_seats:20
      in
      let st = ok (add_room patio st) in
      (* Seats = 30 -> raw_demand 26 fits completely; worker speed = 25 caps
         served *)
      let _, report = simulate_day st in
      assert_equal 26 report.customers_arrived ~printer:pi;
      assert_equal 25 report.customers_served ~printer:pi;
      assert_equal 15 report.upkeep ~printer:pi;
      assert_equal 115 report.wages ~printer:pi );
    ( "multi-day simulation advances days and accumulates money" >:: fun _ ->
      let t0 = Shop.new_game () in
      let t1, _ = simulate_day t0 in
      let t2, _ = simulate_day t1 in
      assert_equal 3 (day t2) ~printer:pi;
      assert_equal 170 (money t2) ~printer:pi (* 100 + 35 + 35 *) );
  ]

let () =
  run_test_tt_main
    ("simulation"
    >::: [ "basic" >::: basic_tests; "scaling" >::: scaling_tests ])
