# CS 3110 Project: Cooking Mamas

A restaurant management game in OCaml. You run a restaurant for 15 days,
earning money each day and spending it between days to upgrade recipes,
hire or train workers, and expand the restaurant.

## Team

- Ethan G.R. Lee - egl54
- Colin Park - cjp277
- Cassidy Ng - ctn27
- Jeffrey Sun - js3867

## Requirements

- OCaml and opam (set up as in the CS 3110 installation instructions)
- dune
- OUnit2, for the tests

If you don't already have dune and OUnit2, install them with:

    opam install dune ounit2

## Building

From the project's root folder (the one containing `dune-project`):

    dune build

## Running the tests

    dune test

This runs both test suites:

- `test/test_cooking_mamas.ml`: the shop catalog, item lookup, and
  recipe/worker upgrades
- `test/test_state.ml`: the game state, and buying, upgrading, and training
  through the shop

Each suite prints `OK` when all of its tests pass.

## Running the game

    dune exec bin/main.exe

The game loop and the day simulation are still in progress, so this doesn't
play a full game yet.

## Project layout

- `lib/state.ml(i)`: core types (recipes, workers, rooms) and the game state
- `lib/shop.ml(i)`: the shop catalog, starting setup, and purchases
- `lib/simulation.ml(i)`: simulating one day of business (in progress)
- `bin/main.ml`: the 15-day game loop and terminal interface (in progress)
- `test/`: OUnit test suites