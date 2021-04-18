# euchre-server


4 sockets connect

after 4 connections are made, euchre game starts

player 1 starts as dealer

each round:
 - [x] deal cards
 - [x] "turn over" top card
 - [] choose trump suit 
   -  [x] if top card selected, dealer chooses a card to give up and receives top card
   -  [] if not, each player gets a chance to choose a suit or pass
   -  [] opportunity to call loner
 - [] 5 trick taking subrounds:
   -  [] leader starts
   -  [] highest card player wins the subround
   -  [] next leader = highest card player
 - [] team with most tricks taken wins the round
   -  [] 1 point if their team called it
   -  [] 2 points if the other team called it
   -  [] 4 points if loner

Input:

s, d, c, h -> spades, diamonds, clubs, hearts

9, 10, j, q, k, a
