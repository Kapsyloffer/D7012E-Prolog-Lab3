/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Christoffer Lindkvist 
%    Student user id  : icoili-9
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl'). 

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

compare_scores(State, P1, P2, Player) :-
    (
		P1 < P2 -> Player = 2 ;
     	P1 > P2 -> Player = 1 ;
		tie(State) %Om de är lika skickar vi de till tie hell.
	 ), !.


%To count and keep track of scores.
countScore([], 0, 0).

countScore([1|T], Player1Score, Player2Score):-
	countScore(T, RestPlayer1Score, Player2Score),
	Player1Score is RestPlayer1Score + 1.

countScore([2|T], Player1Score, Player2Score):-
	countScore(T, Player1Score, RestPlayer2Score),
	Player2Score is RestPlayer2Score + 1.

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

terminal(State):-
	moves(1, State, Player1Moves),
	moves(2, State, Player2Moves),
	Player1Moves == [],
	Player2Moves == [].


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
moves(Player, State,MoveList) :-
	pieces(State, Player, Pieces),
	moves(Player, State, Pieces, UnsortedMoveList),
	(UnsortedMoveList = [] 
	->
		MoveList = [n]
	;
		sort(UnsortedMoveList,MoveList)
	).

moves(Player, State, [Coord|Tail],MoveList) :-
	Opp is (Player mod 2) + 1,
	moves(Player, State, Tail, NextMoves), !,
	(findMoves(State, Player, Opp, Coord,Moves) 
	->
		append(Moves, NextMoves,MoveList) %Moves
	;
		MoveList = NextMoves %No moves
	). 


moves(_, _, [], []).

findMoves(State, Player, Opp, Coord, Moves) :-
	findMoves(State, Player, Opp, Coord, Moves,
	[north, south, east, west, nWest, nEast, sWest, sEast]).

findMoves(State, Player, Opp, Coord,Moves, [Direction|Tail]) :-
	findMoves(State, Player, Opp, Coord, NextMoves, Tail),
	(findMove(State, Player, Opp, Coord, Direction, Player,Move) 
	->
		Moves = [Move|NextMoves] % Found move, add to list.
	;
		Moves = NextMoves % No move.
	). 
findMoves(_, _, _, _, [], []).

%Here we find each legal move n each direction,
findMove(State, Player, Opp, [X, Y], Direction, Prev,Move) :-
	fetchCoord([X, Y], Direction, [Xi, Y1]),
	get(State, [Xi, Y1], Tile),
	\+(Tile = Player),
	(
		Tile = Opp, 
		findMove(State, Player, Opp, [Xi, Y1], Direction, Opp,Move)
		
	;
		Tile = ., 
		Prev = Opp,
		Move = [Xi, Y1]
	).

makeMove(State, Player, Move, NewState) :-
	Opp is (Player mod 2) + 1,
	set(State, S, Move, Player),
	makeMove(S, Player, Opp, Move,
	[north, south, east, west, nWest, nEast, sWest, sEast], NewState).
makeMove(State, Player, Opp, Move, [Direction|Tail], NewState) :-
	(flipTiles(State, Player, Opp, Move, Direction, FlippedState) ->
		S = FlippedState % Update state.
	;
		S = State % Keep old state, none are flipped.
	),
	makeMove(S, Player, Opp, Move, Tail, NewState).
makeMove(State, _, _, _, [], State).


pieces(State, Player, Pieces) :- pieces(State, Player, Pieces, 0, 0).

pieces(_, _, [], X, Y) :- X > 5, Y > 5.

pieces(State, Player, Pieces, X, Y) :- Y > 5,
	pieces(State, Player, Pieces, X + 1, 0), !.

pieces(State, Player, Pieces, X, Y) :-
	Xi is X, Y1 is Y,
	(get(State, [Xi, Y1], Player) ->
		Pieces = [[Xi, Y1]|NextPieces]
	;
		Pieces = NextPieces
	),
	pieces(State, Player, NextPieces, X, Y + 1), !.

fetchCoord([X, Y], Direction, [NewX, NewY]) :-
(
	(Direction = north, NewX = X, NewY is Y - 1);
	(Direction = south, NewX = X, NewY is Y + 1);
	(Direction = east, NewX is X + 1, NewY = Y);
	(Direction = west, NewX is X - 1, NewY = Y);
	(Direction = nWest, NewX is X - 1, NewY is Y - 1);
	(Direction = nEast, NewX is X + 1, NewY is Y - 1);
	(Direction = sWest, NewX is X - 1, NewY is Y + 1);
	(Direction = sEast, NewX is X + 1, NewY is Y + 1)
).



flipTiles(State, Player, _, [X, Y], Direction, State) :-
	fetchCoord([X, Y], Direction, [Xi, Y1]),
	get(State, [Xi, Y1], Player).

flipTiles(State, Player, Opp, [X, Y], Direction, NewState) :-
	fetchCoord([X, Y], Direction, [Xi, Y1]),
	get(State, [Xi, Y1], Tile),
	Tile = Opp, 
	flipTiles(State, Player, Opp, [Xi, Y1], Direction, FlippedState),
	set(FlippedState, NewState, [Xi, Y1], Player).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Player,Move,State,NewState,NextPlayer)%%%%%%%%%%%%%%%%%%%%
%%
%% define nextState(Player,Move,State,NewState,NextPlayer).
%   - given that Player makes Move in State, it determines NewState (i.e. the next
%     state) and NextPlayer (i.e. the next player who will move).

nextState(Player, n, State, State, NextPlayer) :- 
	nextPlayer(State, Player, NextPlayer).

nextState(Player, [X, Y], State, NewState, NextPlayer) :-
	makeMove(State, Player, [X, Y], NewState),
	nextPlayer(NewState, Player, NextPlayer).

nextPlayer(NewState, Player, NextPlayer) :-
    Opp is (Player mod 2) + 1,
    length(NewState, OppMovesLength),
    OppMovesLength = 0,
    NextPlayer = Player.

nextPlayer(_, Player, NextPlayer) :-
    NextPlayer is (Player mod 2) + 1.


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
h(State, -100):- winner(State, 1), !.
h(State, 100) :- winner(State, 2), !.
h(State, 0) :- tie(State), !.
h(State, Val) :-
	countScore(State, P1, P2),
	Val is P2 - P1.



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
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
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
 

todo:-
	!.