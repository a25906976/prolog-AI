:- initialization main, halt.
:- dynamic black/2.
:- dynamic white/2.
:- dynamic chessman/1. 


read_predicates([H|T]) :-
    read_line_to_codes(user_input, H),
    (   H = end_of_file ->
        nowPlayer,chessman(Player),
        (   (bChessNum(0),placeFirst);
        	(alpha_beta(Player,2,-999999,999999,(X,Y),_V),
        	 atomic_list_concat([Player, "(", X,", ",Y, ")"], Fact),
    		 writeln(Fact)
            )
        );
    string_codes(Input, H),
    %writeln(Input),
    term_string(Term, Input),
    assert(Term),
    read_predicates(T)).
read_predicates([]).

placeFirst:-
	writeln("black(8, 8)").

valid(X,Y):- X>0, X<16, Y>0, Y<16.
/*********Chess_Type************************************/

/*********Chess_Type************************************/

/*********SCORE***************************/

score_live4(Player, X,Y,S):-
   live4(Player,X,Y,C), S is C*300.
score_dead4(Player,X,Y,S):-
	dead4(Player,X,Y,C), S is C*20.
score_live3(Player,X,Y,S):-
    live3(Player,X,Y,C), S is C*30.
score_jump3(Player,X,Y,S):-
    jump3(Player,X,Y,C), S is C*8.
score_live2(Player,X,Y,S):-
    live2(Player,X,Y,C), S is C*15.
/*score_live3B(X,Y,S):-
    live_threeB(X,Y,C), S is C*80.
score_dead3(X,Y,S):-
    dead_three(X,Y,C), S is C*55.*/
/*********SCORE***************************/
/****************KMP************************************/
/****************KMP************************************/

bChessNum(C):-
    findall((X,Y),black(X,Y),List), length(List, C).

wChessNum(C):-
    findall((X,Y),white(X,Y),List), length(List, C).

nowPlayer:- 
    bChessNum(BC),wChessNum(WC), not(BC=WC),assert(chessman(white)), !.
nowPlayer:-
    bChessNum(BC),wChessNum(WC), BC=WC, assert(chessman(black)).


other_player(white,black).
other_player(black,white).


getToken(X,Y):- black(X,Y); white(X,Y).

%valid action
action(Player,L):-
    findall((X,Y),(getToken(X1,Y1),between(1,15,X),between(1,15,Y),(abs(X-X1)<2,abs(Y-Y1)<2),\+getToken(X,Y),\+ forbidden(Player,X,Y) ),L1),
    list_to_set(L1, L).
   % \+getToken(X,Y). %還沒加上禁手


    

play_chess((X,Y),S):-
    S=white -> assert(white(X,Y));
    S=black -> assert(black(X,Y)). 

remove_chess((X,Y),S):-
    S=white ->  retractall(white(X,Y));
    S=black ->  retractall(black(X,Y)).
    
    
    
%return a utility max value.

    
maxScore(Player,X,Y,S):-
    score_live4(Player,X,Y,S2), score_dead4(Player, X,Y,S3),
    score_live3(Player,X,Y,S4), score_jump3(Player,X,Y,S1),
    score_live2(Player,X,Y,S5),
    S is S1+S2+S3+S4+S5.
/****************************************/
win(Player):-
    (Player=white,
     white(X,Y),maxChessLen(white,X,Y,C),C>=5,!
    );
    (Player=black,
     black(X,Y),maxChessLen(black,X,Y,C),C=5,!
    ).
score(Chess,700000):- %Chess 要傳chessman(S)的S
    win(Chess),!.
score(Chess,-700000):-
    other_player(Chess,Another), win(Anthoer),!.

score(Chess,Score):-
    Chess=black,
	findall( S1,(black(X,Y),maxScore(black,X,Y,S1)), W),
    sum_list(W, Sum1),
    findall( S1,(white(X,Y),maxScore(white,X,Y,S1)), Z),
    sum_list(Z, Sum2),
    Score is Sum1-Sum2, !.
score(Chess,Score):-
    Chess=white,
    findall(S1,(white(X,Y),maxScore(white,X,Y,S1)), W),
	sum_list(W, Sum1),
    findall( S1,(black(X,Y),maxScore(black,X,Y,S1)), Z),
    sum_list(Z, Sum2),
    Score is Sum1-Sum2, !.
	
value(Player,Value):-
    %chessman(MainP),
    score(Player,Value).
    
    
%getXY((X,Y),(X,Y)).

alpha_beta(Player,0,_Alpha,_Beta,_NoMove,Value) :-
   value(Player,Value).

alpha_beta(Player,D,Alpha,Beta,Move,700001) :-
    D>0,
    action(Player,Moves),
 	(   Player = white ->  (member(Move,Moves), maxChessLen(white,Move,SS),SS>=5);
   		Player = black ->  (member(Move,Moves), maxChessLen(black,Move,SS),SS=5)
    ),!.
  	
alpha_beta(Player,D,Alpha,Beta,Move,Value) :-
   D > 0, 
   action(Player,Moves),
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is D-1, 
   evaluate_and_choose(Player,Moves,D1,Alpha1,Beta1,nil,(Move,Value)).

evaluate_and_choose(Player,[Move|Moves],D,Alpha,Beta,Record,BestMove) :-
   play_chess(Move,Player),
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   remove_chess(Move,Player),
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Record,BestMove).
evaluate_and_choose(_Player,[],_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Record,(Move,Value)) :- 
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,_Record,BestMove) :- 
   Alpha < Value, Value < Beta, !, 
   evaluate_and_choose(Player,Moves,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Record,BestMove) :- 
   Value =< Alpha, !, 
   evaluate_and_choose(Player,Moves,D,Alpha,Beta,Record,BestMove).




/************UP***************************/

blackUpLen(X,Y,0,0):- Y2 is Y+1, \+getToken(X,Y2),!.
blackUpLen(X,Y,0,1):- Y2 is Y+1, (   white(X,Y2);\+valid(X,Y2)),!.
blackUpLen(X,Y,L,B):- Y2 is Y+1, black(X,Y2), blackUpLen(X,Y2,L2,B), L is L2+1.
blackUpLen(X,Y,0):- Y2 is Y+1, \+black(X,Y2),!.
blackUpLen(X,Y,L):- Y2 is Y+1, black(X,Y2), blackUpLen(X,Y2,L2), L is L2+1.

whiteUpLen(X,Y,0,0):- Y2 is Y+1, \+ getToken(X,Y2),!.
whiteUpLen(X,Y,0,1):- Y2 is Y+1, (   black(X,Y2);\+valid(X,Y2)),!.
whiteUpLen(X,Y,L,B):- Y2 is Y+1, white(X,Y2), whiteUpLen(X,Y2,L2,B), L is L2+1.
whiteUpLen(X,Y,0):- Y2 is Y+1, \+white(X,Y2),!.                
whiteUpLen(X,Y,L):- Y2 is Y+1, white(X,Y2), whiteUpLen(X,Y2,L2), L is L2+1.
    
/*************UP**************************/
/*************DOWN***********************/
blackDownLen(X,Y,0,0):- Y2 is Y-1, \+ getToken(X,Y2),!.
blackDownLen(X,Y,0,1):- Y2 is Y-1, (   white(X,Y2);\+valid(X,Y2)),!.
blackDownLen(X,Y,L,B):- Y2 is Y-1, black(X,Y2), blackDownLen(X,Y2,L2,B), L is L2+1.
blackDownLen(X,Y,0):- Y2 is Y-1, \+ black(X,Y2),!.
blackDownLen(X,Y,L):- Y2 is Y-1, black(X,Y2), blackDownLen(X,Y2,L2), L is L2+1.
whiteDownLen(X,Y,0,0):- Y2 is Y-1, \+getToken(X,Y2),!.
whiteDownLen(X,Y,0,1):- Y2 is Y-1, (   black(X,Y2);\+valid(X,Y2)),!.
whiteDownLen(X,Y,L,B):- Y2 is Y-1, white(X,Y2), whiteDownLen(X,Y2,L2,B), L is L2+1.
whiteDownLen(X,Y,0):- Y2 is Y-1, \+ white(X,Y2),!.
whiteDownLen(X,Y,L):- Y2 is Y-1, white(X,Y2), whiteDownLen(X,Y2,L2), L is L2+1.

/*************DOWN***********************/
/************Horizontal*****************/
judgeWhiteCol(X,Y,C):-
    whiteUpLen(X,Y,C1),whiteDownLen(X,Y,C2), C is C1+C2+1.
judgeWhiteCol(X,Y,C,B):-
    whiteUpLen(X,Y,C1,B1),whiteDownLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeblackCol(X,Y,C):-
    blackUpLen(X,Y,C1),blackDownLen(X,Y,C2), C is C1+C2+1.
judgeblackCol(X,Y,C,B):-
    blackUpLen(X,Y,C1,B1),blackDownLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
/************Horizontal*****************/
/***********Left************************/
blackLeftLen(X,Y,0,0):- X2 is X-1, \+getToken(X2,Y),!.
blackLeftLen(X,Y,0,1):- X2 is X-1, (   white(X2,Y);\+valid(X2,Y)),!.
blackLeftLen(X,Y,L,B):- X2 is X-1, black(X2,Y), blackLeftLen(X2,Y,L2,B), L is L2+1.
blackLeftLen(X,Y,0):- X2 is X-1, \+ black(X2,Y),!.
blackLeftLen(X,Y,L):- X2 is X-1, black(X2,Y), blackLeftLen(X2,Y,L2), L is L2+1.
whiteLeftLen(X,Y,0,0):- X2 is X-1, \+getToken(X2,Y),!.
whiteLeftLen(X,Y,0,1):- X2 is X-1, (   black(X2,Y);\+valid(X2,Y)),!.
whiteLeftLen(X,Y,L,B):- X2 is X-1, white(X2,Y), whiteLeftLen(X2,Y,L2,B), L is L2+1.
whiteLeftLen(X,Y,0):- X2 is X-1, \+ white(X2,Y),!.
whiteLeftLen(X,Y,L):- X2 is X-1, white(X2,Y), whiteLeftLen(X2,Y,L2), L is L2+1.
/***********Left************************/
/***********Right***********************/
blackRightLen(X,Y,0,0):- X2 is X+1, \+ getToken(X2,Y),!.
blackRightLen(X,Y,0,1):- X2 is X+1, (   white(X2,Y);\+valid(X2,Y)),!.
blackRightLen(X,Y,L,B):- X2 is X+1, black(X2,Y), blackRightLen(X2,Y,L2,B), L is L2+1.
blackRightLen(X,Y,0):- X2 is X+1, \+ black(X2,Y),!.
blackRightLen(X,Y,L):- X2 is X+1, black(X2,Y), blackRightLen(X2,Y,L2), L is L2+1.
whiteRightLen(X,Y,0,0):- X2 is X+1, \+ getToken(X2,Y),!.
whiteRightLen(X,Y,0,1):- X2 is X+1, (   black(X2,Y);\+valid(X2,Y)),!.
whiteRightLen(X,Y,L,B):- X2 is X+1, white(X2,Y), whiteRightLen(X2,Y,L2,B), L is L2+1.
whiteRightLen(X,Y,0):- X2 is X+1, \+ white(X2,Y),!.
whiteRightLen(X,Y,L):- X2 is X+1, white(X2,Y), whiteRightLen(X2,Y,L2), L is L2+1.
/***********Right***********************/
/***********Vertical********************/
judgeWhiteRow(X,Y,C,B):-
    whiteLeftLen(X,Y,C1,B1),whiteRightLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeWhiteRow(X,Y,C):-
    whiteLeftLen(X,Y,C1),whiteRightLen(X,Y,C2), C is C1+C2+1.
judgeblackRow(X,Y,C,B):-
    blackLeftLen(X,Y,C1,B1),blackRightLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeblackRow(X,Y,C):-
    blackLeftLen(X,Y,C1),blackRightLen(X,Y,C2), C is C1+C2+1.
/***********Vertical********************/
/**********right-bot TO left-top********/
blackRBLTLen(X,Y,0,0):- X2 is X-1, Y2 is Y+1, \+ getToken(X2,Y2),!.
blackRBLTLen(X,Y,0,1):- X2 is X-1, Y2 is Y+1, (   white(X2,Y2);\+valid(X2,Y2)),!.
blackRBLTLen(X,Y,L,B):- X2 is X-1, Y2 is Y+1, black(X2,Y2), blackRBLTLen(X2,Y2,L2,B), L is L2+1.
blackRBLTLen(X,Y,0):- X2 is X-1, Y2 is Y+1, \+ black(X2,Y2),!.
blackRBLTLen(X,Y,L):- X2 is X-1, Y2 is Y+1, black(X2,Y2), blackRBLTLen(X2,Y2,L2), L is L2+1.
whiteRBLTLen(X,Y,0,0):- X2 is X-1, Y2 is Y+1, \+ getToken(X2,Y2),!.
whiteRBLTLen(X,Y,0,1):- X2 is X-1, Y2 is Y+1, (   black(X2,Y2);\+valid(X2,Y2)),!.
whiteRBLTLen(X,Y,L,B):- X2 is X-1, Y2 is Y+1, white(X2,Y2), whiteRBLTLen(X2,Y2,L2,B), L is L2+1.
whiteRBLTLen(X,Y,0):- X2 is X-1, Y2 is Y+1, \+ white(X2,Y2),!.
whiteRBLTLen(X,Y,L):- X2 is X-1, Y2 is Y+1, white(X2,Y2), whiteRBLTLen(X2,Y2,L2), L is L2+1.
/**********right-bot TO left-top********/
/**********left-top TO right-bot********/
blackLTRBLen(X,Y,0,0):- X2 is X+1, Y2 is Y-1, \+ getToken(X2,Y2),!.
blackLTRBLen(X,Y,0,1):- X2 is X+1, Y2 is Y-1, (   white(X2,Y2);\+valid(X2,Y2)),!.
blackLTRBLen(X,Y,L,B):- X2 is X+1, Y2 is Y-1, black(X2,Y2), blackLTRBLen(X2,Y2,L2,B), L is L2+1.
blackLTRBLen(X,Y,0):- X2 is X+1, Y2 is Y-1, \+ black(X2,Y2),!.
blackLTRBLen(X,Y,L):- X2 is X+1, Y2 is Y-1, black(X2,Y2), blackLTRBLen(X2,Y2,L2), L is L2+1.
whiteLTRBLen(X,Y,0,0):- X2 is X+1, Y2 is Y-1, \+ getToken(X2,Y2),!.
whiteLTRBLen(X,Y,0,1):- X2 is X+1, Y2 is Y-1, (   black(X2,Y2); \+valid(X2,Y2)),!.
whiteLTRBLen(X,Y,L,B):- X2 is X+1, Y2 is Y-1, white(X2,Y2), whiteLTRBLen(X2,Y2,L2,B), L is L2+1.
whiteLTRBLen(X,Y,0):- X2 is X+1, Y2 is Y-1, \+ white(X2,Y2),!.
whiteLTRBLen(X,Y,L):- X2 is X+1, Y2 is Y-1, white(X2,Y2), whiteLTRBLen(X2,Y2,L2), L is L2+1.
/**********left-top TO right-bot********/
/**********Main Diagonal****************/
judgeWhiteMD(X,Y,C,B):-
    whiteRBLTLen(X,Y,C1,B1),whiteLTRBLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeWhiteMD(X,Y,C):-
    whiteRBLTLen(X,Y,C1),whiteLTRBLen(X,Y,C2), C is C1+C2+1.
judgeblackMD(X,Y,C,B):-
    blackLTRBLen(X,Y,C1,B1),blackRBLTLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeblackMD(X,Y,C):-
    blackLTRBLen(X,Y,C1),blackRBLTLen(X,Y,C2), C is C1+C2+1.
/**********Main Diagonal****************/
/**************LBRT*********************/
blackLBRTLen(X,Y,0,0):- X2 is X+1, Y2 is Y+1, \+ getToken(X2,Y2),!.
blackLBRTLen(X,Y,0,1):- X2 is X+1, Y2 is Y+1, white(X2,Y2),!.
blackLBRTLen(X,Y,L,B):- X2 is X+1, Y2 is Y+1, black(X2,Y2), blackLBRTLen(X2,Y2,L2,B), L is L2+1.
blackLBRTLen(X,Y,0):- X2 is X+1, Y2 is Y+1, \+ black(X2,Y2),!.
blackLBRTLen(X,Y,L):- X2 is X+1, Y2 is Y+1, black(X2,Y2), blackLBRTLen(X2,Y2,L2), L is L2+1.
whiteLBRTLen(X,Y,0,0):- X2 is X+1, Y2 is Y+1, \+ getToken(X2,Y2),!.
whiteLBRTLen(X,Y,0,1):- X2 is X+1, Y2 is Y+1, black(X2,Y2),!.
whiteLBRTLen(X,Y,L,B):- X2 is X+1, Y2 is Y+1, white(X2,Y2), whiteLBRTLen(X2,Y2,L2,B), L is L2+1.
whiteLBRTLen(X,Y,0):- X2 is X+1, Y2 is Y+1, \+ white(X2,Y2),!.
whiteLBRTLen(X,Y,L):- X2 is X+1, Y2 is Y+1, white(X2,Y2), whiteLBRTLen(X2,Y2,L2), L is L2+1.
/**************LBRT*********************/
/**************RTLB*********************/
blackRTLBLen(X,Y,0,0):- X2 is X-1, Y2 is Y-1, \+ getToken(X2,Y2),!.
blackRTLBLen(X,Y,0,1):- X2 is X-1, Y2 is Y-1, white(X2,Y2),!.
blackRTLBLen(X,Y,L,B):- X2 is X-1, Y2 is Y-1, black(X2,Y2), blackRTLBLen(X2,Y2,L2,B), L is L2+1.
blackRTLBLen(X,Y,0):- X2 is X-1, Y2 is Y-1, \+ black(X2,Y2),!.
blackRTLBLen(X,Y,L):- X2 is X-1, Y2 is Y-1, black(X2,Y2), blackRTLBLen(X2,Y2,L2), L is L2+1.
whiteRTLBLen(X,Y,0,0):- X2 is X-1, Y2 is Y-1, \+ getToken(X2,Y2),!.
whiteRTLBLen(X,Y,0,1):- X2 is X-1, Y2 is Y-1, \+ black(X2,Y2),!.
whiteRTLBLen(X,Y,L,B):- X2 is X-1, Y2 is Y-1, white(X2,Y2), whiteRTLBLen(X2,Y2,L2,B), L is L2+1.
whiteRTLBLen(X,Y,0):- X2 is X-1, Y2 is Y-1, \+ white(X2,Y2),!.
whiteRTLBLen(X,Y,L):- X2 is X-1, Y2 is Y-1, white(X2,Y2), whiteRTLBLen(X2,Y2,L2), L is L2+1.
/**************RTLB*********************/
/**********Vice Diagonal****************/
judgeWhiteVD(X,Y,C,B):-
    whiteLBRTLen(X,Y,C1,B1),whiteRTLBLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeWhiteVD(X,Y,C):-
    whiteLBRTLen(X,Y,C1),whiteRTLBLen(X,Y,C2), C is C1+C2+1.
judgeblackVD(X,Y,C,B):-
    blackLBRTLen(X,Y,C1,B1),blackRTLBLen(X,Y,C2,B2), C is C1+C2+1, B is B1+B2.
judgeblackVD(X,Y,C):-
    blackLBRTLen(X,Y,C1),blackRTLBLen(X,Y,C2), C is C1+C2+1.
/**********Vice Diagonal****************/
live4_row(Player,X,Y,0):-
    Player = white ->  \+judgeWhiteRow(X,Y,4,0);
    Player = black ->  \+judgeblackRow(X,Y,4,0), !.
live4_row(Player,X,Y,1):-
    Player = white ->  judgeWhiteRow(X,Y,4,0);
    Player = black ->  judgeblackRow(X,Y,4,0), !.
live4_col(Player,X,Y,0):-
    Player = white ->  \+judgeWhiteCol(X,Y,4,0);
	Player = black ->  \+judgeblackCol(X,Y,4,0), !.
live4_col(Player,X,Y,1):-
    Player = white ->  judgeWhiteCol(X,Y,4,0);
	Player = black ->  judgeblackCol(X,Y,4,0), !.
live4_MD(Player,X,Y,0):-
    Player = white ->  \+judgeWhiteMD(X,Y,4,0);
    Player = black ->  \+judgeblackMD(X,Y,4,0), !.
live4_MD(Player,X,Y,1):-
    Player = white ->  judgeWhiteMD(X,Y,4,0);
    Player = black ->  judgeblackMD(X,Y,4,0), !.
live4_VD(Player,X,Y,0):-
    Player = white ->  \+judgeWhiteVD(X,Y,4,0);
    Player = black ->  \+judgeblackVD(X,Y,4,0), !.
live4_VD(Player,X,Y,1):-
    Player = white ->  judgeWhiteVD(X,Y,4,0);
    Player = black ->  judgeblackVD(X,Y,4,0), !.

live4(Player,X,Y,Num):-
	live4_row(Player,X,Y,N1), live4_col(Player,X,Y,N2), live4_MD(Player,X,Y,N3), live4_VD(Player,X,Y,N4),
    Num is N1+N2+N3+N4,!.
    
/*****************live4***********************/
dead4_row(Player,X,Y,0):-
    Player = white -> \+  (		whiteLeftLen(X,Y,Lleft,B1), whiteRightLen(X,Y,Lright,B2), 2 > B1+B2,
                            	NXr is X+Lright+2, NXl is X-Lleft-2, judgeWhiteRow(NXr,Y,NLr), judgeWhiteRow(NXl,Y,NLl),
                              ( (	1 is B1+B2, 4 is Lleft+Lright+1);
                              	(   0 is B1+B2, (   (   white(NXr,Y), 4 is Lright+Lleft+NLr+1);(   white(NXl,Y), 4 is Lright+Lleft+NLl+1)));
                              	(   1 is B1, white(NXr,Y), 4 is Lleft+Lright+NLr+1);
                              	(   1 is B2, white(NXl,Y), 4 is Lleft+Lright+NLl+1)
                              )                    
                       );
    Player = black ->  \+ (		blackLeftLen(X,Y,Lleft,B1), blackRightLen(X,Y,Lright,B2), 2 > B1+B2,
                            	NXr is X+Lright+2, NXl is X-Lleft-2, judgeblackRow(NXr,Y,NLr), judgeblackRow(NXl,Y,NLl),
                              ( (	1 is B1+B2, 4 is Lleft+Lright+1);
                              	(   0 is B1+B2, (   (   black(NXr,Y), 4 is Lright+Lleft+NLr+1);(   black(NXl,Y), 4 is Lright+Lleft+NLl+1)));
                              	(   1 is B1, black(NXr,Y), 4 is Lleft+Lright+NLr+1);
                              	(   1 is B2, black(NXl,Y), 4 is Lleft+Lright+NLl+1)
                              )                    
                       ),!.
dead4_row(Player,X,Y,1):-
    Player = white ->   (		whiteLeftLen(X,Y,Lleft,B1), whiteRightLen(X,Y,Lright,B2), 2 > B1+B2,
                            	NXr is X+Lright+2, NXl is X-Lleft-2, judgeWhiteRow(NXr,Y,NLr), judgeWhiteRow(NXl,Y,NLl),
                              ( (	1 is B1+B2, 4 is Lleft+Lright+1);
                              	(   0 is B1+B2, (   (   white(NXr,Y), 4 is Lright+Lleft+NLr+1);(   white(NXl,Y), 4 is Lright+Lleft+NLl+1)));
                              	(   1 is B1, white(NXr,Y), 4 is Lleft+Lright+NLr+1);
                              	(   1 is B2, white(NXl,Y), 4 is Lleft+Lright+NLl+1)
                              )                    
                       );
    Player = black ->   (		blackLeftLen(X,Y,Lleft,B1), blackRightLen(X,Y,Lright,B2), 2 > B1+B2,
                            	NXr is X+Lright+2, NXl is X-Lleft-2, judgeblackRow(NXr,Y,NLr), judgeblackRow(NXl,Y,NLl),
                              ( (	1 is B1+B2, 4 is Lleft+Lright+1);
                              	(   0 is B1+B2, (   (   black(NXr,Y), 4 is Lright+Lleft+NLr+1);(   black(NXl,Y), 4 is Lright+Lleft+NLl+1)));
                              	(   1 is B1, black(NXr,Y), 4 is Lleft+Lright+NLr+1);
                              	(   1 is B2, black(NXl,Y), 4 is Lleft+Lright+NLl+1)
                              )                    
                       ),!.

dead4_col(Player,X,Y,0):-
    Player = white ->  \+ (		whiteUpLen(X,Y,Lup,B1), whiteDownLen(X,Y,Ldown,B2), 2 > B1+B2,
                            	NYd is Y-Ldown-2, NYu is Y+Lup+2, judgeWhiteCol(X,NYd,NLd), judgeWhiteCol(X,NYu,NLu),
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(X,NYd), 4 is Lup+Ldown+NLd+1);(   white(X,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(X,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(X,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black ->  \+ (		blackUpLen(X,Y,Lup,B1), blackDownLen(X,Y,Ldown,B2), 2 > B1+B2,
                            	NYd is Y-Ldown-2, NYu is Y+Lup+2, judgeblackCol(X,NYd,NLd), judgeblackCol(X,NYu,NLu),
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(X,NYd), 4 is Lup+Ldown+NLd+1);(   black(X,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(X,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(X,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!.                     	
dead4_col(Player,X,Y,1):-
    Player = white ->   (		whiteUpLen(X,Y,Lup,B1), whiteDownLen(X,Y,Ldown,B2), 2 > B1+B2,
                            	NYd is Y-Ldown-2, NYu is Y+Lup+2, judgeWhiteCol(X,NYd,NLd), judgeWhiteCol(X,NYu,NLu),
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(X,NYd), 4 is Lup+Ldown+NLd+1);(   white(X,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(X,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(X,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black ->   (		blackUpLen(X,Y,Lup,B1), blackDownLen(X,Y,Ldown,B2), 2 > B1+B2,
                            	NYd is Y-Ldown-2, NYu is Y+Lup+2, judgeblackCol(X,NYd,NLd), judgeblackCol(X,NYu,NLu),
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(X,NYd), 4 is Lup+Ldown+NLd+1);(   black(X,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(X,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(X,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!.
dead4_MD(Player,X,Y,0):-
    Player = white -> \+  (   whiteRBLTLen(X,Y,Lup,B1), whiteLTRBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X-Lup-2, NYu is Y+Lup+2, NXd is X+Ldown+2, NYd is Y-Ldown-2,
                           judgeWhiteMD(NXu,NYu,NLu),judgeWhiteMD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   white(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black -> \+  (   blackRBLTLen(X,Y,Lup,B1), blackLTRBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X-Lup-2, NYu is Y+Lup+2, NXd is X+Ldown+2, NYd is Y-Ldown-2,
                           judgeblackMD(NXu,NYu,NLu),judgeblackMD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   black(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!. 
dead4_MD(Player,X,Y,1):-
    Player = white ->  (   whiteRBLTLen(X,Y,Lup,B1), whiteLTRBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X-Lup-2, NYu is Y+Lup+2, NXd is X+Ldown+2, NYd is Y-Ldown-2,
                           judgeWhiteMD(NXu,NYu,NLu),judgeWhiteMD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   white(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black ->  (   blackRBLTLen(X,Y,Lup,B1), blackLTRBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X-Lup-2, NYu is Y+Lup+2, NXd is X+Ldown+2, NYd is Y-Ldown-2,
                           judgeblackMD(NXu,NYu,NLu),judgeblackMD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   black(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!.

dead4_VD(Player,X,Y,0):-
    Player = white -> \+ (   whiteLBRTLen(X,Y,Lup,B1), whiteRTLBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X+Lup+2, NYu is Y+Lup+2, NXd is X-Ldown-2, NYd is Y-Ldown-2,
                           judgeWhiteVD(NXu,NYu,NLu),judgeWhiteVD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   white(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black -> \+ (   blackLBRTLen(X,Y,Lup,B1), blackRTLBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X+Lup+2, NYu is Y+Lup+2, NXd is X-Ldown-2, NYd is Y-Ldown-2,
                           judgeblackVD(NXu,NYu,NLu),judgeblackVD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   black(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!.

dead4_VD(Player,X,Y,1):-
    Player = white ->  (   whiteLBRTLen(X,Y,Lup,B1), whiteRTLBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X+Lup+2, NYu is Y+Lup+2, NXd is X-Ldown-2, NYd is Y-Ldown-2,
                           judgeWhiteVD(NXu,NYu,NLu),judgeWhiteVD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( white(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   white(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, white(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, white(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       );
    Player = black ->  (   blackLBRTLen(X,Y,Lup,B1), blackRTLBLen(X,Y,Ldown,B2), 2>B1+B2,
                           NXu is X+Lup+2, NYu is Y+Lup+2, NXd is X-Ldown-2, NYd is Y-Ldown-2,
                           judgeblackVD(NXu,NYu,NLu),judgeblackVD(NXd,NYd,NLd), 
                              ( (	1 is B1+B2, 4 is Lup+Ldown+1);
                              	(	0 is B1+B2, ( ( black(NXd,NYd), 4 is Lup+Ldown+NLd+1);(   black(NXu,NYu), 4 is Lup+Ldown+NLu+1)));
                              	(   1 is B1, black(NXd,NYd),4 is Lup+Ldown+NLd+1);
                              	(   1 is B2, black(NXu,NYu),4 is Lup+Ldown+NLu+1)
                              )                    
                       ),!.

dead4(Player,X,Y,Num):-
    dead4_row(Player,X,Y,N1),dead4_col(Player,X,Y,N2),dead4_MD(Player,X,Y,N3),dead4_VD(Player,X,Y,N4),
    Num is N1+N2+N3+N4,!.

live3_row(Player,X,Y,0):- %3連子
    Player = white -> \+ (   whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    3 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y)); 
    Player = black -> \+ (blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    3 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y)),!.
live3_row(Player,X,Y,1):- %3連子
    Player = white -> whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    3 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y); 
    Player = black -> blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    3 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y),!.
live3_col(Player,X,Y,0):-
    Player = white -> \+ (whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd)); 
    Player = black -> \+ (blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd)),!.
live3_col(Player,X,Y,1):-
    Player = white -> whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd); 
    Player = black -> blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd),!.
live3_MD(Player,X,Y,0):-
    Player = white -> \+ (whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)); 
    Player = black -> \+ (blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)),!.
live3_MD(Player,X,Y,1):-
    Player = white -> whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd); 
    Player = black -> blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd),!.
live3_VD(Player,X,Y,0):-
    Player = white -> \+ (whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)); 
    Player = black -> \+ (blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)),!.
live3_VD(Player,X,Y,1):-
    Player = white -> whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd); 
    Player = black -> blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    3 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd),!.
live3(Player,X,Y,Num):-
    live3_row(Player,X,Y,N1),live3_col(Player,X,Y,N2),live3_MD(Player,X,Y,N3),live3_VD(Player,X,Y,N4),
    Num is N1+N2+N3+N4,!.

jump3_row(Player,X,Y,0):-
    Player = white -> \+ (whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    (   ( NXl is X-Lleft-2, white(NXl,Y), judgeWhiteRow(NXl,Y,NL,0), 3 is Lleft+Lright+NL+1);
    	( NXr is X+Lright+2, white(NXr,Y), judgeWhiteRow(NXr,Y,NL,0), 3 is Lleft+Lright+NL+1)
    ));
    Player = black -> \+ (blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    (   ( NXl is X-Lleft-2, black(NXl,Y), judgeblackRow(NXl,Y,NL,0), 3 is Lleft+Lright+NL+1);
    	( NXr is X+Lright+2, black(NXr,Y), judgeblackRow(NXr,Y,NL,0), 3 is Lleft+Lright+NL+1)
    )),!.
jump3_row(Player,X,Y,1):- %3連子
    Player = white -> whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    (   ( NXl is X-Lleft-2, white(NXl,Y), judgeWhiteRow(NXl,Y,NL,0), 3 is Lleft+Lright+NL+1);
    	( NXr is X+Lright+2, white(NXr,Y), judgeWhiteRow(NXr,Y,NL,0), 3 is Lleft+Lright+NL+1)
    );
    Player = black -> blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    (   ( NXl is X-Lleft-2, black(NXl,Y), judgeblackRow(NXl,Y,NL,0), 3 is Lleft+Lright+NL+1);
    	( NXr is X+Lright+2, black(NXr,Y), judgeblackRow(NXr,Y,NL,0), 3 is Lleft+Lright+NL+1)
    ),!.
jump3_col(Player,X,Y,0):-
    Player = white -> \+ (whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    (   ( NYd is Y-Ldown-2, white(X,NYd), judgeWhiteCol(X,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NYu is Y+Lup+2, white(X,NYu), judgeWhiteCol(X,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black -> \+ (blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    (   ( NYd is Y-Ldown-2, black(X,NYd), judgeblackCol(X,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NYu is Y+Lup+2, black(X,NYu), judgeblackCol(X,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3_col(Player,X,Y,1):-
    Player = white ->  (whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    (   ( NYd is Y-Ldown-2, white(X,NYd), judgeWhiteCol(X,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NYu is Y+Lup+2, white(X,NYu), judgeWhiteCol(X,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black ->  (blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    (   ( NYd is Y-Ldown-2, black(X,NYd), judgeblackCol(X,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NYu is Y+Lup+2, black(X,NYu), judgeblackCol(X,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3_MD(Player,X,Y,0):-
    Player = white -> \+ (whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    (   ( NXd is X+Ldown+2, NYd is Y-Ldown-2, white(NXd,NYd), judgeWhiteMD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X-Lup-2, NYu is Y+Lup+2, white(NXu,NYu), judgeWhiteMD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black -> \+ (blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    (   ( NXd is X+Ldown+2, NYd is Y-Ldown-2, black(NXd,NYd), judgeblackMD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X-Lup-2, NYu is Y+Lup+2, black(NXu,NYu), judgeblackMD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3_MD(Player,X,Y,1):-
    Player = white ->  (whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    (   ( NXd is X+Ldown+2, NYd is Y-Ldown-2, white(NXd,NYd), judgeWhiteMD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X-Lup-2, NYu is Y+Lup+2, white(NXu,NYu), judgeWhiteMD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black ->  (blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    (   ( NXd is X+Ldown+2, NYd is Y-Ldown-2, black(NXd,NYd), judgeblackMD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X-Lup-2, NYu is Y+Lup+2, black(NXu,NYu), judgeblackMD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3_VD(Player,X,Y,0):-
    Player = white -> \+ (whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    (   ( NXd is X-Ldown-2, NYd is Y-Ldown-2, white(NXd,NYd), judgeWhiteVD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X+Lup+2, NYu is Y+Lup+2, white(NXu,NYu), judgeWhiteVD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black -> \+ (blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    (   ( NXd is X-Ldown-2, NYd is Y-Ldown-2, black(NXd,NYd), judgeblackVD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X+Lup+2, NYu is Y+Lup+2, black(NXu,NYu), judgeblackVD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3_VD(Player,X,Y,1):-
    Player = white ->  (whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    (   ( NXd is X-Ldown-2, NYd is Y-Ldown-2, white(NXd,NYd), judgeWhiteVD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X+Lup+2, NYu is Y+Lup+2, white(NXu,NYu), judgeWhiteVD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    ));
    Player = black ->  (blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    (   ( NXd is X-Ldown-2, NYd is Y-Ldown-2, black(NXd,NYd), judgeblackVD(NXd,NYd,NL,0), 3 is Lup+Ldown+NL+1);
    	( NXu is X+Lup+2, NYu is Y+Lup+2, black(NXu,NYu), judgeblackVD(NXu,NYu,NL,0), 3 is Lup+Ldown+NL+1)
    )),!.
jump3(Player,X,Y,Num):-
    jump3_row(Player,X,Y,N1), jump3_col(Player,X,Y,N2), jump3_MD(Player,X,Y,N3),jump3_VD(Player,X,Y,N4),
    Num is N1+N2+N3+N4, !.

live2_row(Player,X,Y,0):- %3連子
    Player = white -> \+ (   whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    2 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y)); 
    Player = black -> \+ (blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    2 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y)),!.
live2_row(Player,X,Y,1):- %3連子
    Player = white -> whiteLeftLen(X,Y,Lleft,0),whiteRightLen(X,Y,Lright,0),
    2 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y); 
    Player = black -> blackLeftLen(X,Y,Lleft,0),blackRightLen(X,Y,Lright,0),
    2 is Lleft+Lright+1, NXl is X-Lleft-2, \+getToken(NXl,Y), NXr is X+Lright+2, \+getToken(NXr,Y),!.
live2_col(Player,X,Y,0):-
    Player = white -> \+ (whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd)); 
    Player = black -> \+ (blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd)),!.
live2_col(Player,X,Y,1):-
    Player = white -> whiteUpLen(X,Y,Lup,0),whiteDownLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd); 
    Player = black -> blackUpLen(X,Y,Lup,0),blackDownLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NYu is Y+Lup+2, \+getToken(X,NYu), NYd is Y-Ldown-2, \+getToken(X,NYd),!.
live2_MD(Player,X,Y,0):-
    Player = white -> \+ (whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)); 
    Player = black -> \+ (blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)),!.
live2_MD(Player,X,Y,1):-
    Player = white -> whiteRBLTLen(X,Y,Lup,0),whiteLTRBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd); 
    Player = black -> blackRBLTLen(X,Y,Lup,0),blackLTRBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X-Lup-2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X+Ldown+2, NYd is Y-Ldown-2, \+getToken(NXd,NYd),!.
live2_VD(Player,X,Y,0):-
    Player = white -> \+ (whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)); 
    Player = black -> \+ (blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd)),!.
live2_VD(Player,X,Y,1):-
    Player = white -> whiteLBRTLen(X,Y,Lup,0),whiteRTLBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd); 
    Player = black -> blackLBRTLen(X,Y,Lup,0),blackRTLBLen(X,Y,Ldown,0),
    2 is Lup+Ldown+1, NXu is X+Lup+2, NYu is Y+Lup+2, \+getToken(NXu,NYu), NXd is X-Ldown-2, NYd is Y-Ldown-2, \+getToken(NXd,NYd),!.
live2(Player,X,Y,Num):-
    live2_row(Player,X,Y,N1),live2_col(Player,X,Y,N2),live2_MD(Player,X,Y,N3),live2_VD(Player,X,Y,N4),
    Num is N1+N2+N3+N4,!.
    

forbidden(Player,X,Y):- %長連、四四、雙活三
    Player = white ->  false;
    Player = black -> \+ maxChessLen(black,X,Y,5),  
    (	(maxChessLen(black,X,Y,C),C>5); (live4(black,X,Y,N1),dead4(black,X,Y,N2),N1+N2>=2); (live3(black,X,Y,N1),jump3(black,X,Y,N2), N1+N2>=2)).

maxChessLen(P,X,Y,C):-
     (	P = white -> (   
                               judgeWhiteRow(X,Y,L1),
                                   judgeWhiteCol(X,Y,L2),
                                   judgeWhiteMD(X,Y,L3),
                                   judgeWhiteVD(X,Y,L4), C1 is max(L1,L2), C2 is max(L3,L4), C is max(C1,C2)
                                 );
                  P = black ->  (   judgeblackRow(X,Y,L1),
                                    judgeblackCol(X,Y,L2),
                                    judgeblackMD(X,Y,L3),
                                    judgeblackVD(X,Y,L4), C1 is max(L1,L2), C2 is max(L3,L4), C is max(C1,C2)
                                )
                  ),!.
maxChessLen(P,(X,Y),C):-
     (	P = white -> (   
                               judgeWhiteRow(X,Y,L1),
                                   judgeWhiteCol(X,Y,L2),
                                   judgeWhiteMD(X,Y,L3),
                                   judgeWhiteVD(X,Y,L4), C1 is max(L1,L2), C2 is max(L3,L4), C is max(C1,C2)
                                 );
                  P = black ->  (   judgeblackRow(X,Y,L1),
                                    judgeblackCol(X,Y,L2),
                                    judgeblackMD(X,Y,L3),
                                    judgeblackVD(X,Y,L4), C1 is max(L1,L2), C2 is max(L3,L4), C is max(C1,C2)
                                )
                  ),!.
/************MXACHESSLEN*******************/


main :-
    read_predicates(X).