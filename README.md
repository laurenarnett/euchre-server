# euchre-server

![euchre](euchre.png)

4 sockets connect

after 4 connections are made, euchre game starts

player 1 starts as dealer

each round:
 - [x] deal cards
 - [x] "turn over" top card
 - [x] choose trump suit 
   -  [x] if top card selected, dealer chooses a card to give up and receives top card
   -  [x] if not, each player gets a chance to choose a suit or pass
   -  [ ] opportunity to call loner
 - [x] 5 trick taking subrounds:
   -  [x] leader starts
   -  [x] highest card player wins the subround
   -  [x] next leader = highest card player
 - [x] team with most tricks taken wins the round
   -  [x] 1 point if their team called it
   -  [x] 2 points if the other team called it
   -  [ ] 4 points if loner

Input:

s, d, c, h -> spades, diamonds, clubs, hearts

9, 10, j, q, k, a


