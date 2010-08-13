


%alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
%	(
%	(Depth = 0 , staticval(Pos, Val)); 			% Static value of Pos if Depth is 0, this is as deep as we get
%	moves(Pos ,PosList),            			% not deep enough , calc moves 
 %  	NewDepth is Depth -1,
  % 	boundedbest(PosList, Alpha, Beta, GoodPos, Val, NewDepth)  
%  ),!.


%/* we get here if the game is over before we calced all the levels down
%   we check if the game is over in the currebt depth  */
%alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
%      	staticval(Pos, Val).

%%%%%%%%%%%%

alphabeta(Pos, _, _, _, Val, 0) :-
	staticval(Pos, Val), !.


alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
	moves(Pos ,PosList, Depth, LMoves), !,
   	NewDepth is Depth - 1,
   	boundedbest(PosList, Alpha, Beta, GoodPos, Val, NewDepth).


alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
      	staticval(Pos, Val).



boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
  	alphabeta(Pos, Alpha, Beta, _, Val, Depth),
  	goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

goodenough([], _, _, Pos, Val, Pos, Val, _) :- !.    		% No other candidate

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
  	min_to_move(Pos), Val > Beta, !;                   	% Maximizer attained upper bound
  	max_to_move(Pos), Val < Alpha, !.               	% Minimizer attained lower bound

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
  	newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    		% Refine bounds  
  	boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
  	betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
  	min_to_move(Pos), Val > Alpha, !.                 	% Maximizer increased lower bound 

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
   	max_to_move(Pos), Val < Beta, !.                 	% Minimizer decreased upper bound 

newbounds(Alpha, Beta, _, _, Alpha, Beta).          		% Otherwise bounds unchanged 

betterof(Pos, Val, Pos1, Val1, Pos, Val)  :-        		% Pos better than Pos1 
  	min_to_move(Pos), Val > Val1, !;
  	max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).            	 	% Otherwise Pos1 better


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_infinity(10000).

infinity(1000).


%staticval(Pos, Val) :-
%	position_value_list(Pos, Val).


staticval(pos(_, Val, _), Val).

	



max_to_move(pos(NextToMove, _, _)) :-
	next_to_move(0, NextToMove).

min_to_move(Pos) :-
	not max_to_move(Pos).


moves(Pos, PosList, 1, LMoves) :-
	list_to_pos(Pos, 1),
	next_to_move(1, NextToMove),
	get_best_move(1, NextToMove, LMoves),
	retract_pos(1),
	create_pos_list_from_moves_list(Pos, LMoves, PosList), !.



moves(Pos, PosList, Depth, LMoves) :-
	list_to_pos(Pos, 1),
	next_to_move(1, NextToMove),
	get_all_moves_sorted(1, NextToMove, LMoves),
	retract_pos(1),
	create_pos_list_from_moves_list(Pos, LMoves, PosList).


get_best_move(Index, NextToMove, LMoves) :-
	get_all_moves_sorted(Index, NextToMove, LAllMoves),
	get_first_from_list(LAllMoves, LMoves).

get_first_from_list([], []) :- !.
get_first_from_list([Move | LAllMoves], [Move]) :- !.
	
	

create_pos_list_from_moves_list(Pos, [[_, move(_, FX, FY, TX, TY)] | LMoves], [Pos1 | PosList]) :-
	list_to_pos(Pos, 2),
	make_move(2, FX, FY, TX, TY),
	position_value1(2, Val),
	pos_to_list(2, Pos1, Val),
	retract_pos(2),
	create_pos_list_from_moves_list(Pos, LMoves, PosList), !.

create_pos_list_from_moves_list(_, [], []).




%hex(X, Y, State)

make_move_list(pos(NextToMove, Val, LPos), FX, FY, TX, TY) :-
	other_player(NextToMove, OtherPlayer),
	revert_hex_list(LPos, NextToMove, Val, FX, FY, TX, TY, LNewPos).


revert_hex_list([], _, _, _, _, _, _, []) :- !.

revert_hex_list([hex(FX, FY, NextToMove) | LPos], NextToMove, Val, FX, FY, TX, TY, [hex(FX, FY, empty) | LNewPos]) :-
	far_neighbour(FX, FY, TX, TY),
	revert_hex_list(LPos, NextToMove, Val, FX, FY, TX, TY, LNewPos), !.

revert_hex_list([hex(TX, TY, NextToMove) | LPos], NextToMove, Val, FX, FY, TX, TY, [hex(TX, TY, NextToMove) | LNewPos]) :-
	far_neighbour(FX, FY, TX, TY),
	revert_hex_list(LPos, NextToMove, Val, FX, FY, TX, TY, LNewPos), !.

revert_hex_list([hex(X, Y, NextToMove) | LPos], NextToMove, Val, FX, FY, TX, TY, [hex(TX, TY, NextToMove) | LNewPos]) :-
	close_neighbour(X, Y, TX, TY),
	revert_hex_list(LPos, NextToMove, Val, FX, FY, TX, TY, LNewPos), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check(0).

check(Num) :-
	pos_to_list(0, Pos, Val),
	list_to_pos(Pos, 1),
%	get_all_moves(1, LMoves),
%	findall([Result, move(Index, FX, FY, TX, TY)], (hex(Index, FX, FY, NextToMove),	%***
%						      far_neighbour_hex(Index, FX, FY, TX, TY, empty),
%						      calc_move_value(Index, TX, TY, Result, far)), LMoves).
	retract_pos(1),
	NewNum is Num - 1,
	check(NewNum).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retract_all_moves(Index) :-
	retractall(move(Val, Index, FX, FY, TX, TY, Player)), !.
retract_all_moves(_).

init_all_moves(Index) :-
	retract_all_moves(Index),
	get_all_moves(Index, first, LMovesFirst),
	assert_moves_rec(LMovesFirst, first),
	get_all_moves(Index, second, LMovesSecond),
	assert_moves_rec(LMovesSecond, second).

assert_moves_rec([], _) :- !.
assert_moves_rec([[Val, move(Index, FX, FY, TX, TY)] | LMoves], Player) :-
	assert(move(Val, Index, FX, FY, TX, TY, Player)),
	assert_moves_rec(LMoves, Player).


retract_all_moves_to_hex(Index, X, Y) :-
	retractall(move(_, Index, _, _, X, Y, Player)), !.	
retract_all_moves_to_hex(Index, X, Y).

retract_all_moves_from_hex(Index, X, Y) :-
	retractall(move(_, Index, X, Y, _, _, Player)), !.	
retract_all_moves_from_hex(Index, X, Y).



update_moves_with_move(Index, FX, FY, TX, TY) :-
	close_neighbour(FX, FY, TX, TY), !,
	retract_all_moves_to_hex(Index, TX, TY),
	hex(Index, FX, FY, Player),
	other_player(Player, OtherPlayer),
	findall(hex(X, Y), close_neighbour_hex(Index, TX, TY, X, Y, OtherPlayer), LHex),
	findall(move(Val, Index, X, Y, X1, Y1, OtherPlayer), 
			(member(hex(X, Y), LHex), move(Val, Index, X, Y, X1, Y1, OtherPlayer)),
		LMoves),
	change_move_player(LMoves, Player).


update_moves_with_move(Index, FX, FY, TX, TY) :-
	far_neighbour(FX, FY, TX, TY), !,
	retract_all_moves_to_hex(Index, TX, TY),
	retract_all_moves_from_hex(Index, FX, FY),
	assert_new_moves_to_hex(Index, FX, FY),
	hex(Index, FX, FY, Player),
	other_player(Player, OtherPlayer),
	findall(hex(X, Y), close_neighbour_hex(Index, TX, TY, X, Y, OtherPlayer), LHex),
	findall(move(Val, Index, X, Y, X1, Y1, OtherPlayer), 
			(member(hex(X, Y), LHex), move(Val, Index, X, Y, X1, Y1, OtherPlayer)),
		LMoves),
	change_move_player(LMoves, Player),
	assert_new_moves_from(Index, TX, TY, Player),
	assert(move(-1, Index, TX, TY, FX, FY, Player)).


assert_new_moves_from(Index, TX, TY, Player) :-
	assert_far_moves_from(Index, TX, TY, Player),
	assert_close_moves_from(Index, TX, TY, Player).


assert_far_moves_from(Index, TX, TY, Player) :-
	findall([-1, move(Index, TX, TY, X, Y)], far_neighbour_hex(Index, TX, TY, X, Y, empty), LMoves),
	assert_moves_rec(LMoves, Player).
	

assert_close_moves_from(Index, TX, TY, Player) :-
	findall(hex(X1, Y1), close_neighbour_hex(Index, TX, TY, X1, Y1, empty), LHex),
%	findall([-1, move(Index, TX, TY, X, Y)], 
%			(member(hex(X, Y), LHex), 
%			 not (move(_, Index, FX, FY, X, Y, Player), close_neighbour(FX, FY, X, Y))),
%		LMoves),
%	assert_moves_rec(LMoves).
	assert_close_moves_from_hex_to_list(Index, TX, TY, Player, LHex).


assert_close_moves_from_hex_to_list(_, _, _, _, []) :- !.

assert_close_moves_from_hex_to_list(Index, TX, TY, Player, [hex(X, Y) | LHex]) :-
	move(_, Index, FX, FY, X, Y, Player),
	close_neighbour(FX, FY, X, Y),
	assert_close_moves_from_hex_to_list(Index, TX, TY, Player, LHex), !.


assert_close_moves_from_hex_to_list(Index, TX, TY, Player, [hex(X, Y) | LHex]) :-
	assert(move(_, Index, TX, TY, X, Y, Player)),
	assert_close_moves_from_hex_to_list(Index, TX, TY, Player, LHex), !.



change_move_player([], _) :- !.

change_move_player([move(_, Index, X, Y, X1, Y1, _) | LMoves], Player) :-
	far_neighbour(X, Y, X1, Y1),
	retract(move(_, Index, X, Y, X1, Y1, _)),
	assert(move(-1, Index, X, Y, X1, Y1, Player)),
	change_move_player(LMoves, Player), !.

change_move_player([move(_, Index, X, Y, X1, Y1, _) | LMoves], Player) :-
	close_neighbour(X, Y, X1, Y1),
	retract(move(_, Index, X, Y, X1, Y1, _)),
	not (move(_, Index, FX, FY, X1, Y1, Player), close_neighbour(FX, FY, X1, Y1)), 		% check !
	assert(move(-1, Index, X, Y, X1, Y1, Player)),
	change_move_player(LMoves, Player), !.

change_move_player([M | LMoves], Player) :-
	change_move_player(LMoves, Player).


assert_new_moves_to_hex(Index, FX, FY) :-
	assert_far_moves_to_hex(Index, FX, FY),
	assert_close_moves_to_hex(Index, FX, FY).


assert_far_moves_to_hex(Index, X, Y) :-
	findall(hex(FX, FY, first), far_neighbour_hex(Index, X, Y, FX, FY, first), LHexFirst),
	findall(hex(FX, FY, second), far_neighbour_hex(Index, X, Y, FX, FY, second), LHexSecond),
	append(LHexFirst, LHexSecond, LHex),
	assert_moves_to_dest_rec(Index, X, Y, LHex).
	

assert_close_moves_to_hex(Index, X, Y) :-
	close_neighbour_hex(Index, X, Y, XF, YF, first),
	close_neighbour_hex(Index, X, Y, XS, YS, second),
	assert(move(-1, Index, XF, YF, X, Y, first)),
	assert(move(-1, Index, XS, YS, X, Y, second)), !.
	

assert_close_moves_to_hex(Index, X, Y) :-
	close_neighbour_hex(Index, X, Y, XF, YF, first),
	assert(move(-1, Index, XF, YF, X, Y, first)), !.


assert_close_moves_to_hex(Index, X, Y) :-
	close_neighbour_hex(Index, X, Y, XS, YS, second),
	assert(move(-1, Index, XS, YS, X, Y, second)), !.



assert_moves_to_dest_rec(_, _, _, []) :- !.
assert_moves_to_dest_rec(Index, TX, TY, [hex(X, Y, Player) | LHex]) :-
	assert(move(-1, Index, X, Y, TX, TY, Player)),
	assert_moves_to_dest_rec(Index, TX, TY, LHex).
