# Cracker Barrel


### Problem Space Description

At Cracker Barrel restaurants they have a peg-based solitaire game on their tables to help pass the time while waiting for your food.  It is a puzzle board with 15 holes arranged in a pyramid shape:

![image](https://usercontent2.hubstatic.com/10481505_f120.jpg)

Each of the holes, except one, contains a peg.  The goal of the game is to remove as many pegs as possible from the board until no more moves are possible.  Each move consists of jumping one peg in a straight line over one other peg and into an empty hole.  The peg that was jumped over is then removed.
To represent board states, we will assign a number to each hole, from 1-15:

![image](https://lh5.ggpht.com/hc3WTlWwHTGChHzXfKggZSe_18IMrV0PWAly3fus-rY1NRRXJWw2i6Ehl1PD-MQ5JiQk=w300)



A board state will be represented by a list of positions containing pegs, in sorted order.  For example, an initial state might be:

**(1 2 3 4 6 7 8 9 10 11 12 13 14 15)**

And a solvable end state from this might be:

**(13)**

####Basic Version
Given a starting state and goal state, use an uninformed search algorithm to determine whether the goal state is reachable.  Print out the steps required in forward order.

####Intermediate Version
Implement an alternate (non-rectangular) board geometry.  You could use, for example, a star-shaped board, a plus-shaped board, or a circular board.  The size of the board will affect runtime.

####Advanced Version
 implement a bidirectional search.  Note that keeping track of two large frontiers in a breadth-first style search can result in a huge amount of memory use, so if necessary you can use a simpler puzzle geometry, 10 pegs organized in a triangle.