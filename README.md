# multisweeper-snes

it's minesweeper for the SNES made in pure ASM

it's called multisweeper because I also made a version on FUZE⁴ Nintendo Switch, and it's going to be multiplayer like that version.


-- Changelog? --

Saturday 1 July, 2023
Initial commit, after 3 weeks of undocumented development.

Tuesday 11 July, 2023
Lots of new features;
- The board now gets dynamically loaded to vram on startup, instead of a fixed 10x10 board.
- Board centering now works in hi-res mode.
- The mouse now works in hi-res mode.
- Having no controller plugged in hides the cursor.
- The code that derives screen position from board position, and vice versa, are now subroutines. This will help with things like Super Scope support.
- The cursor now actually starts on the board(mostly important when using mouse), thanks to /\ this.