# euchre-server


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
 - [ ] 5 trick taking subrounds:
   -  [x] leader starts
   -  [x] highest card player wins the subround
   -  [x] next leader = highest card player
 - [] team with most tricks taken wins the round
   -  [] 1 point if their team called it
   -  [] 2 points if the other team called it
   -  [] 4 points if loner

Input:

s, d, c, h -> spades, diamonds, clubs, hearts

9, 10, j, q, k, a


BUG : Jack of leading suit should not be a vaild play if player has a card of leading suit in her hand

      Player should only be able to play cards from their hand
      
      Choose a card to play.
    Valid plays: [(Nine,Spades),(Ten,Clubs)]9c
    Player 3 played (Nine,Clubs).

