(* Types and Boundaries --> food, workers, etc. *)

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
