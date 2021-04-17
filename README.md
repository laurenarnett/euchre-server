# euchre-server


4 sockets connect

after 4 connections are made, euchre game starts

player 1 starts as dealer

each round:
1) deal cards
2) "turn over" top card
3) choose trump suit 
    i) if top card selected, dealer chooses a card to give up and receives top card
    ii) if not, each player gets a chance to choose a suit or pass
    iii) opportunity to call loner
4) 5 trick taking subrounds:
    i) leader starts
    ii) highest card player wins the subround
    iii) next leader = highest card player
5) team with most tricks taken wins the round
    i) 1 point if their team called it
    ii) 2 points if the other team called it
    iii) 4 points if loner

Input:

s, d, c, h -> spades, diamonds, clubs, hearts

9, 10, j, q, k, a
