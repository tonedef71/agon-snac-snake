# agon-snac-snake
Snac-Snake: a variant of the classic game [*Snake*](https://en.wikipedia.org/wiki/Snake_(video_game_genre)) written in [BBC BASIC v3](https://en.wikipedia.org/wiki/BBC_BASIC)

* Originally created for the [3rd Olimex Coding Challenge 2023](https://olimex.wordpress.com/2023/06/02/agonlight-week-programming-challenge-issue-3/)
* Snac-Snake may be played on an [AgonLight retro-computer](https://www.olimex.com/Products/Retro-Computers/AgonLight2/open-source-hardware), the [AgonLight Emulator](https://github.com/astralaster/agon-light-emulator/releases), or the [BBC BASIC SDL](http://www.bbcbasic.co.uk/bbcsdl/).

## Controls
Use the up, down, left, and right arrow keys to control the direction of the non-stop moving snake.  Throughout the game, vertical and/or horizontal portals will appear which the snake may safely pass through to arrive at the immediate opposite end of the playing field.

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
