# ChessAI

My implementation of ASCII chess game with opponent AI.

### Current goals
- [x] Implement chess board and pieces
- [X] Implement pieces moves
- [X] Create opponent that moves pieces randomly
- [ ] Implement checks, checkmates and draws
- [ ] Implement game history output to file
- [ ] Implement opponent AI using minimax criteria ([freecodecamp article](https://www.freecodecamp.org/news/simple-chess-ai-step-by-step-1d55a9266977/), [arXiv:1404.1515](https://arxiv.org/abs/1404.1515), [arXiv:1208.1940](https://arxiv.org/abs/1208.1940))

### How to build and execute
To build this project [Haskell Tool Stack](https://github.com/commercialhaskell/stack) is required.  
```
$ cd /path/to/repository/ChessAI
$ stack build  
$ stack exec path/to/generated/executable/ChessAI
```  
After executing `$ stack build` path to generated executable file will be shown.

### How to play
First you should choose color that you will be playing for.  
`Select color ('B'/'W' or 'q' to quit): `  
You can type any string starting with `b` or `B` to play for Black pieces or string starting with `w` or `W` to play for White pieces. You can quit the game by typing any string starting with `q` or `Q`. When the pieces color is selected the game board will appear.  
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
If you decided to play for White you can choose the way you are going to move your pieces. For example you can type: `e2e4`. This will affect on game board in such way.   
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
Now it says that current move is for Black. Instead of move (`e2e4`) you could type any string starting with `q` or `Q` to quit the game.   

### Save game history to file
To be done soon...
