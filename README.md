# agon-snac-snake ![snac-snake_icon](https://github.com/tonedef71/agon-snac-snake/assets/3978924/b3799768-9e80-44d1-925f-246474965573)

Snac-Snake: a variant of the classic game [*Snake*](https://en.wikipedia.org/wiki/Snake_(video_game_genre)) written in [BBC BASIC v3](https://en.wikipedia.org/wiki/BBC_BASIC)

* Originally created for the [3rd Olimex AgonLight Week Programming Challenge 2023](https://olimex.wordpress.com/2023/06/02/agonlight-week-programming-challenge-issue-3/)
* Snac-Snake may be played on an [AgonLight retro-computer](https://www.olimex.com/Products/Retro-Computers/AgonLight2/open-source-hardware), the [AgonLight Emulator](https://github.com/tomm/fab-agon-emulator/releases), the upcoming [Agon Console8](https://heber.co.uk/agon-console8/), or the [BBC BASIC SDL](http://www.bbcbasic.co.uk/bbcsdl/).

## Demo Playthrough
https://github.com/tonedef71/agon-snac-snake/assets/3978924/10138187-4962-456e-aa14-5da3b76a7628

## Display
Snac-Snake supports multiple display modes on the AgonLight.  The game should be played in a mode that supports a minimum of 16 colors and minimum screen dimensions of 40 columns by 22 rows.

On BBC BASIC SDL, one display mode (MODE 9) is supported.

## Controls
Use the up, down, left, and right arrow keys to control the direction of the non-stop moving snake.  Colliding with a wall kills the snake and ends the game.  Vertical and/or horizontal portals will appear during the game to allow the snake safe passage through to the opposite end of the playing field.

## Scoring
* Each small white pellet eaten increases the length of the snake by one body segment and is scores five points per segment body length of the snake.  Since the minimum size of the snake is two (the head and one body segment), a small white pellet scores ten points at a minimum.
* Colliding with a red, pink, light blue, or green monster kills the snake and ends the game.
* Eating the yellow toadstool is toxic to the snake and ends the game.
* Eating a large hollow white pellet is worth twenty-five points and decreases the length of the snake by one body segment.
* Eating a large solid white pellet is worth twenty-five points and temporarily allows the snake to gobble the monsters which are dark blue or white when vulnerable.
* Eating a dark blue monster scores five hundred points and reduces the length of the snake by one body segment.
* Eating a white monster scores one thousand points and reduces the length of the snake by one body segment.
* Eating a red heart scores one hundred points and increases the length of the snake by two body segments.
* Eating a light blue diamond scores two hundred points and increases the length of the snake by two body segments.
* Eating a green pear scores five hundred points and increases the length of the snake by two body segments.

## High Scores
* The default high score is fifteen hundred points.
* High scores will be saved to a file named `snac-snak.hi` in the same folder as the `snac-snake.bas` file.
* Snac-Snake must be run from the exact folder where the `snac-snak.hi` file resides in order for the saved high scores to be read-in by the game.
