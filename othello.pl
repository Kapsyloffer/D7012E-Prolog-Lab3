/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Christoffer Lindkvist 
%    Student user id  : icoili-9
%
/* ------------------------------------------------------- */



%do not change the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl'). 

%Warning supression.
:- discontiguous moves/4.
:- discontiguous findMoves/6.
:- discontiguous playedMove/6.
:- discontiguous getPlayerPositions/5.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          ✅ initialize(InitialState,InitialPlayer).
%          ✅ winner(State,Player) 
%          ✅ tie(State)
%          ✅ terminal(State) 
%          ✅ moves(Player,State,MoveList)
%          ✅ nextState(Player,Move,State,NewState,NextPlayer)
%          ✅ validmove(Player,State,Proposed)
%          ✅ h(State,Val)  (see question 2 in the handout)
%          ✅ lowerBound(B)
%          ✅ upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlayer). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlayer is the player who moves first. 

initialize(InitialState, InitialPlayer):-
	initBoard(InitialState).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Player) here.  
%     - returns winning player if State is a terminal position and
%     Player has a higher score than the other player 

winner(State, Player):-
	terminal(State),
	countScore(State, P1, P2),
    compare_scores(State, P1, P2, Player).

%Basically what it says on the tin, 
%vi kollar vem som leder.
compare_scores(State, P1, P2, Player) :-
    (
		P1 > P2 -> Player = 2 ;
     	P1 < P2 -> Player = 1 ;
		tie(State) %Om de är lika skickar så vi dem till tie hell.
	 ), !.


%To count and keep track of scores.
countScore([], 0, 0).

%Räkna alla ettor.
countScore([1|T], Player1Score, Player2Score):-
	countScore(T, RestPlayer1Score, Player2Score),
	Player1Score is RestPlayer1Score + 1.

%Räkna alla tvåor.
countScore([2|T], Player1Score, Player2Score):-
	countScore(T, Player1Score, RestPlayer2Score),
	Player2Score is RestPlayer2Score + 1.

%Om varken etta eller tvåa skippar vi.
countScore([_|T], Player1Score, Player2Score):-
	countScore(T, Player1Score, Player2Score).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State):-
	terminal(State),
	countScore(State, Player1, Player2),
	Player1 = Player2, !.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

%Terminal betyder att inga fler drag kan göras.
terminal(State):-
	moves(1, State, Player1Moves),
	moves(2, State, Player2Moves),
	Player1Moves == [n],
	Player2Moves == [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Player,State,MoveList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Player,State,MoveList). 
%   - returns list MoveList of all legal moves Player can make in State
%


moves(_, _, [], []).
%Här får vi ut alla möjliga moves genom att:
%1. Få alla positioner av våra stenar
%2. se abdra moves
moves(Player, State, MoveList) :-
	getPlayerPositions(State, Player, Pieces), %Get all pieces
	moves(Player, State, Pieces, List),
	%Om listan är tom så har vi n moves.
	(List = [], MoveList = [n] ; MoveList = List).

%For each stenar kollar vi varje möjlig direction med findmoves
moves(Player, State, [CurPiece|Rest], MoveList) :-
	Opp is (Player mod 2) + 1,
	moves(Player, State, Rest, NextMoves), !,
	(findMoves(State, Player, Opp, CurPiece, Moves), append(Moves, NextMoves, MoveList) ; MoveList = NextMoves). 

%Base case, ništa
findMoves(_, _, _, _, [], []).
%I find moves trycker vi in alla 8 directions i vår findmoves
findMoves(State, Player, Opp, CurPiece, Moves) :-
	findMoves(State, Player, Opp, CurPiece, Moves,
	[north, south, east, west, nWest, nEast, sWest, sEast]). %<--- Bättre göra såhär än 8 separata funktioner.

%En andra findmoves för att hantera varje element i listan
findMoves(State, Player, Opp, CurPiece, Moves, [Direction|Rest]) :-
	findMoves(State, Player, Opp, CurPiece, NextMoves, Rest),
	%Om findmoveshelper returnar true (vi har ett move) så appendar vi det till vår NextMoves list.
	(findMovesHelper(State, Player, Opp, CurPiece, Direction, Player, Move), append([Move], NextMoves, Moves) 
	%Annars fortsätter vi på en annan direction
	; Moves = NextMoves).

% I vår helper hittar vi ett legal move i en riktining.
% Börjar med player,
% Hittar ett led av opponent pieces
% Då vi stöter på en tom plats så är moved valid.
%Då returnar vi true, annars false.
findMovesHelper(State, Player, Opp, [X, Y], Direction, Last, Move) :-
	fetchCoord([X, Y], Direction, [X1, Y1]),
	get(State, [X1, Y1], Tile),
	(\+(Tile = Player), %Om vi inte stöter på en till playertile, fortsätt.
		Tile = Opp, 
		findMovesHelper(State, Player, Opp, [X1, Y1], Direction, Opp, Move)
	; %Om den är tom, och tidigare tilen i riktningen var en opponent, då är [X1, Y1] en valid move.
		Tile = ., %<--- tom ruta
		Last = Opp,
		Move = [X1, Y1]).


playedMove(State, _, _, _, [], State).
%Vi tar en evaluated valid move, och spelar ut den.
playedMove(State, Player, Move, NewState) :-
	Opp is (Player mod 2) + 1,
	set(State, S, Move, Player), %Sätter ut brickan.
	%Likt findmoves så tar vi alla directions från vårt moves, och checkar om något måste flippas.
	playedMove(S, Player, Opp, Move, [north, south, east, west, nWest, nEast, sWest, sEast], NewState).

playedMove(State, Player, Opp, Move, [Direction|Rest], NewState) :-
	%Om fliptiles returnar true så har vi flippat tiles i en riktning och sätter det till vår nya state,
	%Annars så är state densamma för inget flippas.
	(flipTiles(State, Player, Opp, Move, Direction, FlippedState), S = FlippedState ; S = State ),
	playedMove(S, Player, Opp, Move, Rest, NewState).

%Om vi når slutet av brädet så terminatear vi.
getPlayerPositions(_, _, [], X, Y) :- 
	X > 5, 
	Y > 5.
%Hämtar coordinaterna på varje spelpjäs
% Spelpjäs being en bricka som är vit elelr svart (1 eller 2)
getPlayerPositions(State, Player, Pieces) :- 
	getPlayerPositions(State, Player, Pieces, 0, 0).

%Vi skannar efter pieces kolumn efter kolumn
%och ger oss spelarens positioner.
%Exempel ordning i en 4x4:
% 					y↓ x→
% x = 0
% 01 05 09 12	y = 0
% 02 06 10 13
% 03 07 11 14
% 04 08 12 15 	y = 3
%		   x = 3
getPlayerPositions(State, Player, Pieces, X, Y) :- 
	Y > 5,
	getPlayerPositions(State, Player, Pieces, X + 1, 0), !.

getPlayerPositions(State, Player, Pieces, X, Y) :-
	X1 is X, 
	Y1 is Y,
	%Om vi hittat en piece så hamnar den i listan, annars fortsätter vi utan den.
	(get(State, [X1, Y1], Player), Pieces = [[X1, Y1]|NextPieces] ; Pieces = NextPieces),
	getPlayerPositions(State, Player, NextPieces, X, Y + 1), !.
%See above comment för dokumentation.


%En lista som beskriver åt vilket håll varje direction ligger
%		 NORTH			
%	NW			NE
% WEST	[Piece]	 EAST
%	SW			SE
%		 SOUTH
fetchCoord([X, Y], Direction, [X1, Y1]) :-
(
	(Direction = north, X1 is X    , 	Y1 is Y - 1); 
	(Direction = south, X1 is X    , 	Y1 is Y + 1);
	(Direction = east, 	X1 is X + 1, 	Y1 is Y	   );
	(Direction = west, 	X1 is X - 1, 	Y1 is Y    );
	(Direction = nWest, X1 is X - 1, 	Y1 is Y - 1);
	(Direction = nEast, X1 is X + 1, 	Y1 is Y - 1);
	(Direction = sWest, X1 is X - 1, 	Y1 is Y + 1);
	(Direction = sEast, X1 is X + 1, 	Y1 is Y + 1)
). 
%^This is silly but I kinda like it.

%Base case: Player hittad på andra sidan riktningen:
%Och då avslutar vi så att vi inte råkar flippa hela boardet. :P
flipTiles(State, Player, _, [X, Y], Direction, State) :-
	fetchCoord([X, Y], Direction, [X1, Y1]),
	get(State, [X1, Y1], Player).

%Denna kommer flippa i en direction i den nya staten tills
%Den stöter på en player, om vi inte slår i en player 
%eller om opponents tar slut så
%returnar den false och vi testar en annan direction.
flipTiles(State, Player, Opp, [X, Y], Direction, NewState) :-
	fetchCoord([X, Y], Direction, [X1, Y1]),
	get(State, [X1, Y1], Tile),
	Tile = Opp, 
	flipTiles(State, Player, Opp, [X1, Y1], Direction, FlippedState),
	set(FlippedState, NewState, [X1, Y1], Player).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Player,Move,State,NewState,NextPlayer)%%%%%%%%%%%%%%%%%%%%
%%
%% define nextState(Player,Move,State,NewState,NextPlayer).
%   - given that Player makes Move in State, it determines NewState (i.e. the next
%     state) and NextPlayer (i.e. the next player who will move).

%Om vi tvingas spela move n OCH 
%det inte är terminal så:
%byter vi bara spelare.
%ANNARS: avslutas spelet.
nextState(Player, n, State, State, NextPlayer) :- 
	\+ terminal(State),
	nextPlayer(Player, NextPlayer).

%Men om vi kör en valid move (räknas i play) så gör vi ett move
nextState(Player, [X, Y], State, NewState, NextPlayer) :-
	playedMove(State, Player, [X, Y], NewState),
	nextPlayer(Player, NextPlayer).

%Ger oss opponent.
nextPlayer(Player, NextPlayer) :-
    NextPlayer is (Player mod 2) + 1.

%OBS: Dessa kallas enbart av play.pl om en valid move är spelad. ^


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Player,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Player,State,Proposed). 
%   - true if Proposed move by Player is valid at State.

validmove(Player, State, Proposed):-
	moves(Player, State, MoveList),
	member(Proposed, MoveList).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(_,0).
%Om h går mot -100 så vinner 1
h(State, -100):- 
	winner(State, 1), !.
%Annars så vinner 2
h(State, 100) :- 
	winner(State, 2), !.
%Och om 0 så är det lika
h(State, 0) :- 
	tie(State), !.
%Val är skillnaden av scores mellan spelarna
h(State, Val) :-
	countScore(State, P1, P2),
	Val is P2 - P1.
%Tbh jag har ingen aning om hur det här funkar, Hjalmar hjälpte mig här.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indeX1ng starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indeX1ng starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 