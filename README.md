# ChessAI

My implementation of a console chess game with opponent AI.

### Current goals
- [x] Implement chess board and pieces
- [X] Implement pieces moves
- [X] Create opponent that moves pieces randomly
- [ ] Implement checks, checkmates and draws
- [ ] Implement game history output to file
- [ ] Implement opponent AI using minimax criteria ([freecodecamp article](https://www.freecodecamp.org/news/simple-chess-ai-step-by-step-1d55a9266977/), [arXiv:1404.1515](https://arxiv.org/abs/1404.1515), [arXiv:1208.1940](https://arxiv.org/abs/1208.1940))

### Requirements
Tested on Ubuntu 20.04.5 LTS.
- [Haskell stack](https://docs.haskellstack.org/).

### How to run
To run the game you can simply run the shell-script `run.sh`.

### How to play
First you should choose the color of your pieces.  
`Select color ('B'/'W' or 'q' to quit): `  
You can type any string starting with `b` or `B` to play for Black pieces or string starting with `w` or `W` to play for White pieces. You can quit the game by typing any string starting with `q` or `Q`. When the color is selected the game board will appear.
```
  A B C D E F G H
 ┏━┯━┯━┯━┯━┯━┯━┯━┓
8┃♜│♞│♝│♛│♚│♝│♞│♜┃8
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
7┃♟│♟│♟│♟│♟│♟│♟│♟┃7
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
6┃▫│▪│▫│▪│▫│▪│▫│▪┃6
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
5┃▪│▫│▪│▫│▪│▫│▪│▫┃5
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
4┃▫│▪│▫│▪│▫│▪│▫│▪┃4
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
3┃▪│▫│▪│▫│▪│▫│▪│▫┃3
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
2┃♙│♙│♙│♙│♙│♙│♙│♙┃2
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
1┃♖│♘│♗│♕│♔│♗│♘│♖┃1
 ┗━┷━┷━┷━┷━┷━┷━┷━┛
  A B C D E F G H

Move for White.
```  
If you decided to play for White you can choose the way you are going to move your pieces. For example you can type: `e2e4`. This will affect on the game board in the following way.
```
  A B C D E F G H
 ┏━┯━┯━┯━┯━┯━┯━┯━┓
8┃♜│♞│♝│♛│♚│♝│♞│♜┃8
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
7┃♟│♟│♟│♟│♟│♟│♟│♟┃7
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
6┃▫│▪│▫│▪│▫│▪│▫│▪┃6
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
5┃▪│▫│▪│▫│▪│▫│▪│▫┃5
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
4┃▫│▪│▫│▪│♙│▪│▫│▪┃4
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
3┃▪│▫│▪│▫│▪│▫│▪│▫┃3
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
2┃♙│♙│♙│♙│▫│♙│♙│♙┃2
 ┠─┼─┼─┼─┼─┼─┼─┼─┨
1┃♖│♘│♗│♕│♔│♗│♘│♖┃1
 ┗━┷━┷━┷━┷━┷━┷━┷━┛
  A B C D E F G H

Move for Black.
```  
Now it says that the current move is for Black. Instead of move you could type any string starting with `q` or `Q` to quit the game.   

### Save game history to file
To be done soon...
