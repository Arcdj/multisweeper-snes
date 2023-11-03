# multisweeper-snes

it's Minesweeper for the SNES made in pure Assembly.

It's called multisweeper because there are multiple control options, it's multi-platform as I also made a version for FUZE⁴ Nintendo Switch, and it's multiplayer like that version.

## Changelog

Saturday 1 July 2023

Initial commit, after 3 weeks of undocumented development.

Tuesday 11 July 2023

Lots of new features;

- The board now gets dynamically loaded to vram on startup, instead of a fixed 10x10 board.
- Board centering now works in hi-res mode.
- The mouse now works in hi-res mode.
- Having no controller plugged in hides the cursor.
- The code that derives screen position from board position, and vice versa, are now subroutines. This will help with things like Super Scope support.
- The cursor now actually starts on the board(mostly important when using mouse), thanks to /\ this.

Saturday 15 July 2023
Multiplayer is here!

- Added Super Scope support too. It's an absolute pain to use in hi-res mode. Currently, it can play outside the bottom and right edges of the board, this will (probably) get fixed later. Because the Super Scope is very weird, it's got a very weird control sceme. Let me know if you have any better ideas!
- You can now mix and match any combination of joypads, mice, and up to one Super Scope, up to 2 controllers at once, at any time.
- Made the background color brighter, this makes the Super Scope work at the edges of smaller boards. It doesn't work very well at the edges of larger boards.
- The cursors are now always hidden on the first frame, so they don't appear on only that frame when nothing's plugged in.

Friday 3 November 2023
Some small changes, mostly under the hood.

- Functions are now in a seperate source file from the main file.
- Made a 16-bit binary to decimal function, using hardware division by 10.
- Thanks to this, you can now see the number of mines remaining above the board.
- I also added a purple 9, just for this
- Added a rainbow function, that'll be fun
- You can now hold the flag button down to place (or remove) multiple flags more quickly, without having to press it each time.
