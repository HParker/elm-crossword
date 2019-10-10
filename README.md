# Easy build and upload crossword puzzles

# Design doc:

Puzzle:
- title
- author
- height
- width
- has_many clues

Clue:
- Direction
- Number
- x
- y
- Answer
- Clue

Pages:
- Puzzle List
- Builder Page
- Solver Page

Player:
- Selected square
- Selection direction
- ? active clue
