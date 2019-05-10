# pokemonstrous
"What if we could give Pokemon the **Moneyball treatment**?" That was the question that led to Pokemonstrous.

Pokemonstrous (when all the features are there) will be useful for simulating battles in various formats and crunching data about the results. It isn't meant to be a PvP library, really, though you could use it for that if you wanted to. Its main purpose is to simulate thousands or millions of battles at high speed and compare the results. To that end, I would divide its development into four rough phases:

1. An in-memory, persistent object database, using the data from [PokeAPI](https://github.com/PokeAPI/pokeapi). (Status: **Complete**)
2. A fully-featured battle simulator. (Status: **Early development**)
3. Data analysis of battle results. (Status: **Not started**)
4. Intelligent battling -- as opposed to completely random battling, which is what happens now. (Status: **Not started**)

To use Pokemonstrous, you should ideally know your way around a Lisp REPL. The instructions below are fairly newb-friendly, but a complete Lisp tutorial is outside the scope of this doc.

## Installation
0. Copy the .pmdb database directory to ~/.pmdb or some other location, then make sure `*datastore*` (in main.lisp) matches that. You can use it from the source directory if you want, but if you make any database changes, you'll get conflicts when you `git pull`.
1. Start a Lisp REPL. I've only tested with SBCL, but it should work in other implementations.
2. Load pokemonstrous.asd
3. From the REPL: `(asdf:load-system :pokemonstrous)` (Dependencies: bknr.datastore)
4. Then: `(bknr.datastore:open-store pm:*datastore*)`
5. You can now use Pokemonstrous! Currently, the only "useful" thing you can do that doesn't require poking around in code is to watch a (very) random battle with `(pm:random-battle)`.

## Missing battle features
The TODO list, in rough priority order:
* Stat changes
* Statuses
* Full implementation of moves (using individual functions)
* Abilities
* Held items
* Field effects, such as weather
* Mega Evolutions
* Z-moves
* Double battles
* Triple battles

These features are already supported by the scaffolding -- for example, every Pokemon, item, ability, and move up through Ultra Sun/Ultra Moon is already in the database. It's just a matter of when I feel like working on the battle stuff. ;-)

## Rebuilding the database
If you're contributing, or doing your own customizations, or you just don't trust me, you might need to rebuild the database:

0. Follow the instructions to set up your own local [PokeAPI](https://github.com/PokeAPI/pokeapi) database. (For the love of Pikachu, please do not use the public PokeAPI endpoint for this. That would be very rude and probably get you banned.)
1. Do steps 1 and 2 above, then: `(asdf:load-system :pokemonstrous.build)`. (Dependencies: drakma, jonathan)
2. `(bknr.datastore:open-store pm:*datastore*)`
3. `(pm:build-db)`. This may take **15-30 minutes**, depending on your machine.
4. Taking a snapshot will make it faster to load later: `(bknr.datastore:snapshot)`
