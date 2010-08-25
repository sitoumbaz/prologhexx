

color(black, [0, 0, 0]).
color(red, [255, 125, 125]).
color(blue, [125, 125, 255]).
color(yellow, [255, 255, 0]).
color(white, [255, 255, 255]).
color(purple, [225, 200, 240]).
color(green, [0, 255, 0]).
color(gray,  [175, 175, 175]).
color(orange,  [230, 255, 0]).


color_first(red).
color_second(blue).
color_border(black).
color_empty(gray).
color_none(purple).
color_close_neighbour(green).
color_far_neighbour(yellow).
color_selected_hex(yellow).
color_text(white).



init_data :-
	init_next_to_move.


init_next_to_move :-
	retractall(next_to_move(_, _)),
	assert(next_to_move(0, first)), !.
init_next_to_move :-
	assert(next_to_move(0, first)), !.
	



other_player(first, second).
other_player(second, first).


player_color(first, Color) :-
	color_first(Color).

player_color(second, Color) :-
	color_second(Color).


round(X, RoundX) :-
	A is X - X // 1,
	A >= 0.5, !,
	RoundX is X // 1 + 1.
round(X, RoundX) :-
	RoundX is X // 1.


%board_size(7).

board_hex_size(30).

sel_hex_size(SSize) :-
	board_hex_size(BSize),
	SSize is BSize - 2.


calc_window_size(WindowWidth, WindowHeight) :-
	board_size(BSize),
	board_hex_size(HSize),
	WW is 2 * BSize * 3 * HSize / 2,
	round(WW, WindowWidth),
	sqrt3(Sqrt3),
	WH is (2 * BSize - 1) * Sqrt3 * HSize + HSize,
	round(WH, WindowHeight).


show_hex :-
	DStyle = [ws_caption,ws_border,dlg_ownedbyprolog],
	CStyle = [ws_child,ws_visible],
	show_options,
	init_data,
	calc_window_size(WindowWidth, WindowHeight),
	DialogHeight is WindowHeight + 70,
  	wdcreate(hex, `Prolog - Hexxagon`, 100, 100, WindowWidth, DialogHeight, DStyle),
  	wccreate((hex,1), grafix, ``, 0, 0, WindowWidth, WindowHeight, CStyle),
	wgfxmap((hex, 1), 1, 1, 1, 1),
	wsize((hex, 1), X, Y, X1, Y1),
	window_handler(hex, hex_handler),
	insert_all_hex(HexList),
	ButtonY is WindowHeight + 5,
	ButtonWidth is WindowWidth - 25,
%	wccreate((hex, 2), button, `Move`, 10, YYY, 200, 26, CStyle),
%	YYYY is YYY + 30,
	wccreate((hex, 3), button, `End Game`, 10, ButtonY, ButtonWidth, 26, CStyle),
%	wccreate((hex, 4), button, `Exit`, 120, YYYY, 100, 26, CStyle),
	change_colors,
	show_dialog(hex),
	retractall(hex(_, X, Y, State)),
	check_starting_player.


check_starting_player :-
	firstPlayer(computer),	
	play_best_move.


retract_pos(Index) :-
	retractall(next_to_move(Index, _)),
	retractall(hex(Index, _, _, _)), !.
retract_pos(_) :- !.


pos_to_list(Index, pos(NextToMove, Val, LPos), Val) :-
	next_to_move(Index, NextToMove),
	findall(hex(X, Y, State), hex(Index, X, Y, State), LPos).

list_to_pos(pos(NextToMove, _, LPos), Index) :-
	retract_pos(Index),
	assert(next_to_move(Index, NextToMove)),
	assert_pos_rec(LPos, Index).

assert_pos_rec([], _).

assert_pos_rec([hex(X, Y, State) | LPos], Index) :-
	assert(hex(Index, X, Y, State)),
	assert_pos_rec(LPos, Index).



	
	
		


%setof(hex(X, Y, S), X/Y ^ hex(X, Y, S), L).

%findall(LHex, (hex(X, Y, _), draw_single_hex(X, Y, 30, LHex, _)), L).

is_valid_hex(Index, X, Y) :-
	hex(Index, X, Y, State),
	not State = none.


change_colors :-
	board_size(3),
	change_hex_state(0, 3, 1, first),
	change_hex_state(0, 3, 9, second),
	change_hex_state(0, 3, 5, none), !.


change_colors :-
	board_size(5),
	change_hex_state(0, 5, 1, first),
	change_hex_state(0, 9, 13, first),
	change_hex_state(0, 1, 13, first),
	change_hex_state(0, 9, 5, second),
	change_hex_state(0, 5, 17, second),
	change_hex_state(0, 1, 5, second),
	change_hex_state(0, 4, 8, none),
	change_hex_state(0, 6, 8, none),
	change_hex_state(0, 5, 11, none), !.


change_colors :-
	board_size(7),
	change_hex_state(0, 7, 1, first),
	change_hex_state(0, 1, 7, second),
	change_hex_state(0, 7, 7, none).

%	change_selected_hex(8, 8).

change_hex_state(Index, X, Y, NewState) :-
	is_valid_hex(Index, X, Y), !,
	retract(hex(Index, X, Y, _)),
	assert(hex(Index, X, Y, NewState)), !.	
change_hex_state(Index, X, Y, NewState).



retract_selected_hex :-
	retractall(selected_hex(_, _)), !.
retract_selected_hex.


change_selected_hex(X, Y) :-
	selected_hex(X, Y), !,
	retract_selected_hex.

change_selected_hex(X, Y) :-
	retract_selected_hex,
	is_valid_hex(0, X, Y), !,
	assert(selected_hex(X, Y)).
change_selected_hex(X, Y).


create_board_commands(Commands) :-
%	change_colors,
	color_border(CBorder),
	color(CBorder, [CBorderR, CBorderG, CBorderB]),
	color_none(CNone),
	color(CNone, [CNoneR, CNoneG, CNoneB]),
	color_empty(CEmpty),
	color(CEmpty, [CEmptyR, CEmptyG, CEmptyB]),
	color_first(CFirst),
	color(CFirst, [CFirstR, CFirstG, CFirstB]),
	color_second(CSecond),
	color(CSecond, [CSecondR, CSecondG, CSecondB]),
	color_selected_hex(CSelHex),
	color(CSelHex, [CSelHexR, CCSelHexG, CCSelHexB]),
	color_close_neighbour(CCloseNeighbour),
	color(CCloseNeighbour, [CCloseNeighbourR, CCloseNeighbourG, CCloseNeighbourB]),
	color_far_neighbour(CFarNeighbour),
	color(CFarNeighbour, [CFarNeighbourR, CFarNeighbourG, CFarNeighbourB]),
	findall(LHex, (hex(0, HX, HY, _), draw_single_hex(HX, HY, LHex)), LBorders),
	findall(LFill, (hex(0, HX, HY, empty), fill_single_hex(HX, HY, LFill)), LEmpty),
	findall(LFill, (hex(0, HX, HY, none), fill_single_hex(HX, HY, LFill)), LNone),
	findall(LFill, (hex(0, HX, HY, first), fill_single_hex(HX, HY, LFill)), LFirst),
	findall(LFill, (hex(0, HX, HY, second), fill_single_hex(HX, HY, LFill)), LSecond),
	append([pen(CBorderR, CBorderG, CBorderB, 2)], LBorders, LAfterBorders),
	append(LAfterBorders, [brsh(CNoneR, CNoneG, CNoneB, 0), fill(1, 1, CBorderR, CBorderG, CBorderB)], LAfterBackground),
	append(LAfterBackground, [brsh(CEmptyR, CEmptyG, CEmptyB, 0)], LAfterEmptyBrush),
	append(LAfterEmptyBrush, LEmpty, LAfterEmpty),
	append(LAfterEmpty, [brsh(CNoneR, CNoneG, CNoneB, 0)], LAfterNoneBrush),
	append(LAfterNoneBrush, LNone, LAfterNone),
	append(LAfterNone, [brsh(CFirstR, CFirstG, CFirstB, 0)], LAfterFirstBrush),
	append(LAfterFirstBrush, LFirst, LAfterFirst),
	append(LAfterFirst, [brsh(CSecondR, CSecondG, CSecondB, 0)], LAfterSecondBrush),
	append(LAfterSecondBrush, LSecond, LAfterSecond),
	(
		selected_hex(SX, SY), !,
		sel_hex_size(SSize),
		findall(LHex, draw_single_hex(SX, SY, SSize, LHex), LSelHex),
		findall(LHex, (close_neighbour_hex(0, SX, SY, X, Y, empty), draw_single_hex(X, Y, SSize, LHex)), LCloseNeighbour),
		findall(LHex, (far_neighbour_hex(0, SX, SY, X, Y, empty), draw_single_hex(X, Y, SSize, LHex)), LFarNeighbour),
		append(LAfterSecond, [pen(CFarNeighbourR, CFarNeighbourG, CFarNeighbourB, 2)], LAfterFarPen),
		append(LAfterFarPen, LFarNeighbour, LAfterFar),
		append(LAfterFar, [pen(CCloseNeighbourR, CCloseNeighbourG, CCloseNeighbourB, 2)], LAfterClosePen),
		append(LAfterClosePen, LCloseNeighbour, LAfterClose),
		append(LAfterClose, [pen(CSelHexR, CCSelHexG, CCSelHexB, 2)], LAfterSelectedPen),
		append(LAfterSelectedPen, LSelHex, List)
		;
		List = LAfterSecond
	),
	create_metadata_list(LMetadata),
	append(List, LMetadata, Commands).
%	create_moves_commands(LMovesCommands),
%	Commands = List.


create_metadata_list(LMetadata) :-
	board_hex_size(HSize1),
	HSize is HSize1 - 2,
	color_border(CBorder),
	color(CBorder, [CBorderR, CBorderG, CBorderB]),
%	color(white, [CBorderR, CBorderG, CBorderB]),
	color_first(CFirst),
	color(CFirst, [CFirstR, CFirstG, CFirstB]),
	color_second(CSecond),
	color(CSecond, [CSecondR, CSecondG, CSecondB]),
	color_empty(CEmpty),
	color(CEmpty, [CEmptyR, CEmptyG, CEmptyB]),
	color_text(CText),
	color(CText, [CTextR, CTextG, CTextB]),
	warea((hex, 1), XL, YT, W, H),
	DisFromBorderFirst is HSize + 5,
	draw_single_hex_lines(DisFromBorderFirst, DisFromBorderFirst, HSize, LinesFirst),
	DisFromBorderSecond is W - HSize - 5,
	draw_single_hex_lines(DisFromBorderSecond, DisFromBorderFirst, HSize, LinesSecond),
	DisFromBorderEmpty is H - HSize - 5,
	draw_single_hex_lines(DisFromBorderFirst, DisFromBorderEmpty, HSize, LinesEmpty),
	draw_single_hex_lines(DisFromBorderSecond, DisFromBorderEmpty, HSize, LinesTurn),
	next_to_move(0, NextToMove),
	player_color(NextToMove, CNextToMove),
	color(CNextToMove, [CNextToMoveR, CNextToMoveG, CNextToMoveB]),
	DisFromBorderFirstText is DisFromBorderFirst - 6,
	DisFromBorderSecondText is DisFromBorderSecond - 6,
	DisFromBorderEmptyText is DisFromBorderEmpty - 6,
	findall(_, hex(0, _, _, first), LFirstCount),
	findall(_, hex(0, _, _, second), LSecondCount),
	findall(_, hex(0, _, _, empty), LEmptyCount),
	length(LFirstCount, FirstCount),
	length(LSecondCount, SecondCount),
	length(LEmptyCount, EmptyCount),
	number_string(FirstCount, FirstCountStr),
	number_string(SecondCount, SecondCountStr),
	number_string(EmptyCount, EmptyCountStr),
	DisNextTurnText is DisFromBorderSecond - 15,
		position_value(0, Val),
		number_string(Val, ValStr),
	LMetadata = [fore(CTextR, CTextG, CTextB),
		     pen(CBorderR, CBorderG, CBorderB, 2),
		     LinesFirst,
		     LinesSecond,
		     LinesEmpty,
		     LinesTurn,
		     brsh(CFirstR, CFirstG, CFirstB, 0),
		     fill(DisFromBorderFirst, DisFromBorderFirst, CBorderR, CBorderG, CBorderB),
		     back(CFirstR, CFirstG, CFirstB),
		     text(DisFromBorderFirstText, DisFromBorderFirstText, FirstCountStr),
		     brsh(CSecondR, CSecondG, CSecondB, 0),
		     fill(DisFromBorderSecond, DisFromBorderFirst, CBorderR, CBorderG, CBorderB),
		     back(CSecondR, CSecondG, CSecondB),
		     text(DisFromBorderSecondText, DisFromBorderFirstText, SecondCountStr),
		     brsh(CEmptyR, CEmptyG, CEmptyB, 0),
		     fill(DisFromBorderFirst, DisFromBorderEmpty, CBorderR, CBorderG, CBorderB),
		     back(CEmptyR, CEmptyG, CEmptyB),
		     text(DisFromBorderFirstText, DisFromBorderEmptyText, EmptyCountStr),
		     brsh(CNextToMoveR, CNextToMoveG, CNextToMoveB, 0),
		     fill(DisFromBorderSecond, DisFromBorderEmpty, CBorderR, CBorderG, CBorderB),
		     back(CNextToMoveR, CNextToMoveG, CNextToMoveB),
		     text(DisNextTurnText, DisFromBorderEmptyText, `Next`)].
			%text(DisNextTurnText, DisFromBorderEmptyText, ValStr)].
	



%create_moves_commands(LMovesCommands),


close_neighbour_hex(Index, X, Y, NX, NY, State) :-
	(NX is X,     NY is Y + 2, hex(Index, NX, NY, State));
	(NX is X + 1, NY is Y + 1, hex(Index, NX, NY, State));
	(NX is X + 1, NY is Y - 1, hex(Index, NX, NY, State));
	(NX is X,     NY is Y - 2, hex(Index, NX, NY, State));
	(NX is X - 1, NY is Y - 1, hex(Index, NX, NY, State));
	(NX is X - 1, NY is Y + 1, hex(Index, NX, NY, State)).

far_neighbour_hex(Index, X, Y, NX, NY, State) :-
	(NX is X,     NY is Y + 4, hex(Index, NX, NY, State));
	(NX is X + 1, NY is Y + 3, hex(Index, NX, NY, State));
	(NX is X + 2, NY is Y + 2, hex(Index, NX, NY, State));
	(NX is X + 2, NY is Y,     hex(Index, NX, NY, State));
	(NX is X + 2, NY is Y - 2, hex(Index, NX, NY, State));
	(NX is X + 1, NY is Y - 3, hex(Index, NX, NY, State));
	(NX is X,     NY is Y - 4, hex(Index, NX, NY, State));
	(NX is X - 1, NY is Y - 3, hex(Index, NX, NY, State));
	(NX is X - 2, NY is Y - 2, hex(Index, NX, NY, State));
	(NX is X - 2, NY is Y,     hex(Index, NX, NY, State));
	(NX is X - 2, NY is Y + 2, hex(Index, NX, NY, State));
	(NX is X - 1, NY is Y + 3, hex(Index, NX, NY, State)).


% can return 'none' hexes !!!

close_neighbour(X, Y, NX, NY) :-
	(NX is X,     NY is Y + 2, hex(0, NX, NY, _));
	(NX is X + 1, NY is Y + 1, hex(0, NX, NY, _));
	(NX is X + 1, NY is Y - 1, hex(0, NX, NY, _));
	(NX is X,     NY is Y - 2, hex(0, NX, NY, _));
	(NX is X - 1, NY is Y - 1, hex(0, NX, NY, _));
	(NX is X - 1, NY is Y + 1, hex(0, NX, NY, _)).

far_neighbour(X, Y, NX, NY) :-
	(NX is X,     NY is Y + 4, hex(0, NX, NY, _));
	(NX is X + 1, NY is Y + 3, hex(0, NX, NY, _));
	(NX is X + 2, NY is Y + 2, hex(0, NX, NY, _));
	(NX is X + 2, NY is Y,     hex(0, NX, NY, _));
	(NX is X + 2, NY is Y - 2, hex(0, NX, NY, _));
	(NX is X + 1, NY is Y - 3, hex(0, NX, NY, _));
	(NX is X,     NY is Y - 4, hex(0, NX, NY, _));
	(NX is X - 1, NY is Y - 3, hex(0, NX, NY, _));
	(NX is X - 2, NY is Y - 2, hex(0, NX, NY, _));
	(NX is X - 2, NY is Y,     hex(0, NX, NY, _));
	(NX is X - 2, NY is Y + 2, hex(0, NX, NY, _));
	(NX is X - 1, NY is Y + 3, hex(0, NX, NY, _)).


	

make_move(Index, FX, FY, TX, TY) :-
	close_neighbour_hex(Index, FX, FY, TX, TY, empty),
	revert_hexes_after_move(Index, TX, TY), !.

make_move(Index, FX, FY, TX, TY) :-
	far_neighbour_hex(Index, FX, FY, TX, TY, empty),
	change_hex_state(Index, FX, FY, empty),
	revert_hexes_after_move(Index, TX, TY), !.

make_move(Index, FX, FY, TX, TY).


revert_hexes_after_move(Index, TX, TY):-
	next_to_move(Index, NextToMove),
	change_hex_state(Index, TX, TY, NextToMove),
	other_player(NextToMove, OtherPlayer),
	findall(pos(X, Y), close_neighbour_hex(Index, TX, TY, X, Y, OtherPlayer), LHexes),
	revert_hex_list(Index, LHexes, NextToMove),
	change_next_to_move(Index).

revert_hex_list(_, [], _) :- !.

revert_hex_list(Index, [pos(X, Y) | LHexes], NextToMove) :-
	change_hex_state(Index, X, Y, NextToMove),	
	revert_hex_list(Index, LHexes, NextToMove).


change_next_to_move(Index) :-
	next_to_move(Index, NextToMove),
	other_player(NextToMove, OtherPlayer),
	retract(next_to_move(Index, NextToMove)),
	assert(next_to_move(Index, OtherPlayer)), !.

get_all_moves(Index, Player, LMoves) :-
	get_all_far_moves(Index, Player, LFar),
	get_all_close_moves(Index, Player, LClose),
	append(LClose, LFar, LMoves), !.

get_all_moves(_, _, []).


remove_0_moves_rec([], []).

remove_0_moves_rec([[0, Move] | LAllMoves], LMoves) :-
	remove_0_moves_rec(LAllMoves, LMoves), !.

remove_0_moves_rec([[R, Move] | LAllMoves], [[R, Move] | LMoves]) :-
	remove_0_moves_rec(LAllMoves, LMoves), !.
	

%get_all_moves_sorted_without_0(Index, LSortedMoves) :-
%	get_all_moves(Index, LAllMoves),
%	remove_0_moves_rec(LAllMoves, LMoves),
%	sort(LMoves, LSortedMovesReversed),
%	reverse(LSortedMovesReversed, LSortedMoves).



get_all_moves_sorted(Index, Player, LSortedMoves) :-
	get_all_moves(Index, Player, LAllMoves),
	length(LAllMoves, Len),
	Len > 0,
%	remove_0_moves_rec(LAllMoves, LMoves),
%	sort(LMoves, LSortedMovesReversed),
	sort(LAllMoves, LSortedMovesReversed),
	reverse(LSortedMovesReversed, LSortedMoves).



get_all_far_moves(Index, Player, LMoves) :-
	findall([Result, move(Index, FX, FY, TX, TY)], (hex(Index, FX, FY, Player),	%***
						      far_neighbour_hex(Index, FX, FY, TX, TY, empty),
						      calc_move_value(Index, TX, TY, Result, far)), LMoves), !.

get_all_far_moves(_, _, []).


get_all_close_moves(Index, Player, LMoves) :-
	setof(pos(Index, X, Y), X1 ^ Y1 ^ (hex(Index, X, Y, empty),
			   close_neighbour_hex(Index, X, Y, X1, Y1, Player)), LPos),
	create_close_moves(LPos, Player, LMoves), !.

get_all_close_moves(_, _, []).


create_close_moves([], _, []).

create_close_moves([pos(Index, X, Y) | LPos], Player, [[Result, move(Index, FX, FY, TX, TY)] | LMoves]) :-	%***
	get_one_close_neighbour(Index, X, Y, Player, [Result, move(Index, FX, FY, TX, TY)]),									%***
	calc_move_value(Index, TX, TY, Result, close),
	create_close_moves(LPos, Player, LMoves).	




get_one_close_neighbour(Index, TX, TY, Player, [_, move(Index, FX, FY, TX, TY)]) :-			%***
	!, close_neighbour_hex(Index, TX, TY, FX, FY, Player).





calc_move_value(Index, TX, TY, Result, close) :-
	count_neighbours(Index, TX, TY, Count),
	Result is Count * 2 + 1, !.

calc_move_value(Index, TX, TY, Result, far) :-
	count_neighbours(Index, TX, TY, Count),
	Result is Count * 2, !.
	

count_neighbours(Index, TX, TY, Count) :-
	next_to_move(Index, NextToMove),
	other_player(NextToMove, OtherPlayer),
	findall(_, close_neighbour_hex(Index, TX, TY, _, _, OtherPlayer), LHex),
	length(LHex, Count).
	


has_moves(Index, Player) :-
	hex(Index, FX, FY, Player),
	far_neighbour_hex(Index, FX, FY, TX, TY, empty), !.

has_moves(Index, Player) :-
	hex(Index, FX, FY, Player),
	close_neighbour_hex(Index, FX, FY, TX, TY, empty), !.




calc_position_hex(Index, NextToMoveCount, OtherPlayerCount, EmptyCount) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	findall(_, hex(Index, _, _, NextToMove), LNextToMove),
	length(LNextToMove, NextToMoveCount),
	findall(_, hex(Index, _, _, OtherPlayer), LOtherPlayer),
	length(LOtherPlayer, OtherPlayerCount),
	findall(_, hex(Index, _, _, empty), LEmpty),
	length(LEmpty, EmptyCount).

position_value(Index, Val) :-
	calc_position_hex(Index, NextToMoveCount, OtherPlayerCount, EmptyCount),
	position_value_inner(Index, NextToMoveCount, OtherPlayerCount, EmptyCount, Val).


position_value_inner(Index, NextToMoveCount, OtherPlayerCount, 0 /*EmptyCount*/, Val) :-
	calc_end_game_val(NextToMoveCount, OtherPlayerCount, Val), !.

position_value_inner(Index, NextToMoveCount, OtherPlayerCount, EmptyCount, Val) :-
	next_to_move(0, NextToMove),
	not has_moves(Index, NextToMove),		% the next player to move has no moves.
	OtherPlayerCountNew is OtherPlayerCount + EmptyCount,
	calc_end_game_val(NextToMoveCount, OtherPlayerCountNew, Val), !.
	%Val is NextToMoveCount - OtherPlayerCount - EmptyCount, !.


position_value_inner(Index, NextToMoveCount, OtherPlayerCount, EmptyCount, Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	not has_moves(Index, OtherPlayer),		% the other player to move has no moves.
	NextToMoveCountNew is NextToMoveCount + EmptyCount,
	calc_end_game_val(NextToMoveCountNew, OtherPlayerCount, Val), !.
	%Val is NextToMoveCount - OtherPlayerCount + EmptyCount, !.

position_value_inner(Index, NextToMoveCount, OtherPlayerCount, EmptyCount, Val) :-
	Val is NextToMoveCount - OtherPlayerCount, !.

/*
position_value1(Index, Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	not has_moves(Index, NextToMove),		% the next player to move has no moves.
	has_moves(Index, OtherPlayer),
	findall(_, hex(Index, _, _, NextToMove), LNextToMove),
	length(LNextToMove, NextToMoveCount),
	findall(_, hex(Index, _, _, OtherPlayer), LOtherPlayer),
	length(LOtherPlayer, OtherPlayerCount),
	findall(_, hex(Index, _, _, empty), LEmpty),
	length(LEmpty, EmptyCount),
	Val is NextToMoveCount - OtherPlayerCount - EmptyCount, !.


position_value1(Index, Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	not has_moves(Index, OtherPlayer),		% the other player to move has no moves.
	has_moves(Index, NextToMove),
	findall(_, hex(Index, _, _, NextToMove), LNextToMove),
	length(LNextToMove, NextToMoveCount),
	findall(_, hex(Index, _, _, OtherPlayer), LOtherPlayer),
	length(LOtherPlayer, OtherPlayerCount),
	findall(_, hex(Index, _, _, empty), LEmpty),
	length(LEmpty, EmptyCount),
	Val is NextToMoveCount - OtherPlayerCount + EmptyCount, !.



position_value1(Index, Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	not has_moves(Index, OtherPlayer),		% both players can't move.
	not has_moves(Index, NextToMove),		% game over.
	findall(_, hex(Index, _, _, NextToMove), LNextToMove),
	length(LNextToMove, NextToMoveCount),
	findall(_, hex(Index, _, _, OtherPlayer), LOtherPlayer),
	length(LOtherPlayer, OtherPlayerCount),
	calc_end_game_val(NextToMoveCount, OtherPlayerCount, Val), !.


position_value1(Index, Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	findall(_, hex(Index, _, _, NextToMove), LNextToMove),
	length(LNextToMove, NextToMoveCount),
	findall(_, hex(Index, _, _, OtherPlayer), LOtherPlayer),
	length(LOtherPlayer, OtherPlayerCount),
	Val is NextToMoveCount - OtherPlayerCount, !.
*/

% The next to move player won.
calc_end_game_val(ValNext, ValOther, Val) :-
	ValNext > ValOther,
	infinity(Inf),
	Val is Inf + ValNext - ValOther, !.

% The next to move player lost.
calc_end_game_val(ValNext, ValOther, Val) :-
	ValNext < ValOther,
	infinity(Inf),
	Val is -Inf + ValNext - ValOther, !.

% a tie game.
calc_end_game_val(Count, Count, 0).


%is_game_over(Index, 


position_value_list(pos(_, _, LPos), Val) :-
	next_to_move(0, NextToMove),
	other_player(NextToMove, OtherPlayer),
	position_value_rec(LPos, NextToMove, OtherPlayer, ValNext, ValOther),
	calc_val(ValNext, ValOther, Val).


calc_val(0, _, -100) :- !.

calc_val(_, 0, 100) :- !.

calc_val(ValNext, ValOther, Val) :-
	Val is ValNext - ValOther, !.

position_value_rec([], _, _, 0, 0) :- !.

position_value_rec([hex(X, Y, NextToMove) | LPos], NextToMove, OtherPlayer, NewVal, ValOther) :-
	position_value_rec(LPos, NextToMove, OtherPlayer, Val, ValOther),
	NewVal is Val + 1, !.

position_value_rec([hex(X, Y, OtherPlayer) | LPos], NextToMove, OtherPlayer, ValNext, NewVal) :-
	position_value_rec(LPos, NextToMove, OtherPlayer, ValNext, Val),
	NewVal is Val + 1, !.

position_value_rec([hex(X, Y, State) | LPos], NextToMove, OtherPlayer, ValNext, ValOther) :-
	position_value_rec(LPos, NextToMove, OtherPlayer, ValNext, ValOther), !.




sqrt3(1.7320508075688772935274463415059).

hex_handler(hex, msg_close, Data, close).

/*
hex_handler((hex, 4), msg_button, _, close) :-
	msgbox(`Hexxagon - Exit`, `Are you sure you want to exit the game ?`, 36, Ans),
	Ans = 6,	% Yes pressed.
	show_score.
*/



hex_handler((hex, 3), msg_button, _, close) :-
	msgbox(`Hexxagon`, `Are you sure you want to end this game?`, 36, Ans ),
	Ans = 6,	% Yes pressed.
	show_score,
	wclose(hex),
	show_hex.


hex_handler(Win, Msg ,Data, Result) :-
	hex_handler(Win, Msg, Data).


hex_handler((hex, 1), msg_paint, _) :-
	refresh_board.

refresh_board :-
	create_board_commands(Commands),
	wgfx((hex, 1), Commands).

/*
hex_handler((hex, 2), msg_button, _) :-
	play_best_move,
	play_next.
*/


show_score :-
	score_msg(Msg),
	msgbox('Game Over', Msg, 0, _).	
	
score_msg(Msg) :-
	findall(_, hex(0, _, _, first), LFirstCount),
	findall(_, hex(0, _, _, second), LSecondCount),
	length(LFirstCount, FirstCount),
	length(LSecondCount, SecondCount),
	check_winner(FirstCount, SecondCount, Msg).

check_winner(Count, Count, Msg) :-
	number_string(Count, CountStr),
	cat([`A tie game, `, CountStr, ` : `, CountStr, `.`], Msg, _), !.

check_winner(FirstCount, SecondCount, Msg) :-
	FirstCount > SecondCount, !,
	number_string(FirstCount, FirstCountStr),
	number_string(SecondCount, SecondCountStr),
	color_first(Color),
	atom_string(Color, ColorStr),
	cat([`The `, ColorStr, ` player won, `, FirstCountStr, ` : `, SecondCountStr, `.`], Msg, _).


check_winner(FirstCount, SecondCount, Msg) :-
	number_string(FirstCount, FirstCountStr),
	number_string(SecondCount, SecondCountStr),
	color_second(Color),
	atom_string(Color, ColorStr),
	cat([`The `, ColorStr, ` player won, `, SecondCountStr, ` : `, FirstCountStr, `.`], Msg, _).




play_best_move :-
	position_value(0, PosVal),
	pos_to_list(0, CurrPos, PosVal),
	start_infinity(Inf),
	MInf is -Inf,
	gameLevel(GameLevel),
	alphabeta(CurrPos, -Inf, Inf, GoodPos, Val, GameLevel),
	retract_pos(0),
	list_to_pos(GoodPos, 0),
%	wgfxpnt((hex, 1)),
	refresh_board,
	can_player_move.



hex_handler((hex ,1), msg_leftup, (X, Y)) :-
%	get_clicked_hex(X, Y, HX, HY), !,
	handle_hex_click(X, Y).
%	change_selected_hex(HX, HY),
%	wgfxpnt((hex, 1)).
%	msgbox(`The Result`, '123', 0, _).	


handle_hex_click(ClickX, ClickY) :-
	selected_hex(SX, SY),
	is_click_inside(ClickX, ClickY, SX, SY),
	change_selected_hex(SX, SY),
	wgfxpnt((hex, 1)), !.

handle_hex_click(ClickX, ClickY) :-
	selected_hex(SX, SY),
	is_click_inside_neghbour(ClickX, ClickY, SX, SY, HX, HY),
	change_selected_hex(SX, SY),
	make_move(0, SX, SY, HX, HY),
%	wgfxpnt((hex, 1)),
	refresh_board,
	can_player_move.
%	!,
%	play_next.



% no hex selected yet.
handle_hex_click(ClickX, ClickY) :-	
	get_clicked_hex(ClickX, ClickY, HX, HY), !,
	change_selected_hex(HX, HY),
	wgfxpnt((hex, 1)).



play_next :-
	next_to_move(0, Player),
	player_check(Player, computer),
	play_best_move, !.
	
play_next.




	
can_player_move :-
	findall(_, hex(0, _, _, empty), []),
	show_score,
	ask_to_play_again, !.	


can_player_move :-
	next_to_move(0, Player),
%	get_all_moves(0, Player, LAllMoves),
%	length(LAllMoves, Len),
%	Len < 1, !,
	not has_moves(0, Player), !,
	player_color(Player, Color),
	atom_string(Color, ColorStr),
	cat([`The `, ColorStr, ` player has no moves.`], Msg, _),
	msgbox(`Game Over`, Msg, 0, _),
	other_player(Player, OtherPlayer),
	update_empty_hex_to_winner(OtherPlayer),
	refresh_board,
	show_score,
	ask_to_play_again, !.

can_player_move :-
	play_next.


ask_to_play_again :-
	wclose(hex),
	msgbox(`Play Again`, `Do you want to play another game ?`, 36, Ans),	
	Ans = 7.	% No pressed.


ask_to_play_again :-
	show_hex.

ask_to_play_again.



update_empty_hex_to_winner(Player) :-
	findall(hex(0, X, Y, empty), hex(0, X, Y, empty), L),
	update_empty_hex_to_winner_list(L, Player).

update_empty_hex_to_winner_list([], _).

update_empty_hex_to_winner_list([hex(0, X, Y, empty) | L], Player) :-
	retract(hex(0, X, Y, empty)),
	assert(hex(0, X, Y, Player)),
	update_empty_hex_to_winner_list(L, Player).




is_click_inside_neghbour(ClickX, ClickY, SX, SY, HX, HY) :-
	is_click_inside_close_neghbour(ClickX, ClickY, SX, SY, HX, HY), !.

is_click_inside_neghbour(ClickX, ClickY, SX, SY, HX, HY) :-
	is_click_inside_far_neghbour(ClickX, ClickY, SX, SY, HX, HY), !.



is_click_inside_close_neghbour(ClickX, ClickY, SX, SY, HX, HY) :-
	close_neighbour_hex(0, SX, SY, HX, HY, empty),
	is_click_inside(ClickX, ClickY, HX, HY).
				
is_click_inside_far_neghbour(ClickX, ClickY, SX, SY, HX, HY) :-
	far_neighbour_hex(0, SX, SY, HX, HY, empty),
	is_click_inside(ClickX, ClickY, HX, HY).


get_clicked_hex(ClickX, ClickY, HX, HY) :-
	next_to_move(0, NextToMove),
	hex(0, HX, HY, NextToMove),
	is_click_inside(ClickX, ClickY, HX, HY).


is_click_inside(ClickX, ClickY, HX, HY) :-
	warea((hex, 1), XL, YT, W, H),
	board_hex_size(HSide),
	calc_hex_pos(HX, HY, HSide, X, Y),
	NewHSide is HSide - 3,
	RevClickY is H - ClickY,
	check_bottom_line(Y, NewHSide, RevClickY),
	check_top_line(Y, NewHSide, RevClickY),
	check_bottom_right_line(X, Y, NewHSide, ClickX, RevClickY),
	check_bottom_left_line(X, Y, NewHSide, ClickX, RevClickY),
	check_top_right_line(X, Y, NewHSide, ClickX, RevClickY),
	check_top_left_line(X, Y, NewHSide, ClickX, RevClickY).


check_top_left_line(X, Y, NewHSide, ClickX, ClickY) :-
	sqrt3(Sqrt3),	
	TX is X - NewHSide,
	TY is Y,
	M is Sqrt3,
	NY is M * (ClickX - TX) + TY,
	NY > ClickY.

check_top_right_line(X, Y, NewHSide, ClickX, ClickY) :-
	sqrt3(Sqrt3),	
	TX is X + NewHSide,
	TY is Y,
	M is -Sqrt3,
	NY is M * (ClickX - TX) + TY,
	NY > ClickY.


check_bottom_left_line(X, Y, NewHSide, ClickX, ClickY) :-
	sqrt3(Sqrt3),	
	TX is X - NewHSide,
	TY is Y,
	M is -Sqrt3,
	NY is M * (ClickX - TX) + TY,
	NY < ClickY.

check_bottom_right_line(X, Y, NewHSide, ClickX, ClickY) :-
	sqrt3(Sqrt3),	
	TX is X + NewHSide,
	TY is Y,
	M is Sqrt3,
	NY is M * (ClickX - TX) + TY,
	NY < ClickY.


check_bottom_line(Y, NewHexSideSize, ClickY) :-
	sqrt3(Sqrt3),
	TY is Y - (NewHexSideSize * Sqrt3) / 2,
	ClickY > TY.


check_top_line(Y, NewHexSideSize, ClickY) :-
	sqrt3(Sqrt3),
	TY is Y + (NewHexSideSize * Sqrt3) / 2,
	ClickY < TY.




fill_single_hex(HX, HY, fill(X, RevY, CBorderR, CBorderG, CBorderB)) :-
	color_border(CBorder),
	color(CBorder, [CBorderR, CBorderG, CBorderB]),
	board_hex_size(BSize),
	calc_hex_pos(HX, HY, BSize, TX, TY),
	warea((hex, 1), XL, YT, W, H),
	TRevY is H - TY,
	round(TX, X),
	round(TRevY, RevY).
	

draw_single_hex(HX, HY, LHex) :-
	board_hex_size(BSize),
	draw_single_hex(HX, HY, BSize, LHex).

draw_single_hex(HX, HY, SSize, LHex) :-
	warea((hex, 1), XL, YT, W, H),
	board_hex_size(BSize),
	calc_hex_pos(HX, HY, BSize, X, Y),
	RevY is H - Y,
	draw_single_hex_lines(X, RevY, SSize, LHex).

draw_single_hex_lines(X, Y, A,
			line(X1, Y1, X2, Y2, X3, Y3, X4, Y4, X5, Y5, X6, Y6, X1, Y1)) :-
	sqrt3(Sqrt3),
	TX1 is X - A / 2,
	TY1 is Y + (A * Sqrt3) / 2,
	TX2 is X + A / 2,
	TY2 is Y + (A * Sqrt3) / 2,
	TX3 is X + A,
	TY3 is Y,
	TX4 is X + A / 2,
	TY4 is Y - (A * Sqrt3) / 2,
	TX5 is X - A / 2,
	TY5 is Y - (A * Sqrt3) / 2,
	TX6 is X - A,
	TY6 is Y,
	round(TX1, X1),
	round(TY1, Y1),
	round(TX2, X2),
	round(TY2, Y2),
	round(TX3, X3),
	round(TY3, Y3),
	round(TX4, X4),
	round(TY4, Y4),
	round(TX5, X5),
	round(TY5, Y5),
	round(TX6, X6),
	round(TY6, Y6).
	



calc_hex_pos(HX, HY, A, X, Y) :-
	sqrt3(Sqrt3),
	X is HX * A * 3 / 2,
	Y is (HY * A * Sqrt3 + A) / 2.


%create_lines_list([], []) :- !.

%create_lines_list([hex(HX, HY) | HexList], [HexLines | LinesList]) :-
%	board_hex_size(BSize),
%	calc_hex_pos(HX, HY, BSize, X, Y),
%	draw_single_hex(X, Y, HexLines),
%	create_lines_list(HexList, LinesList).




insert_single_sum_hex(S, Min, Max, L, NewL) :-
	B is S - Min,
	insert_single_sum_hex_rec(Min, B, Min, Max, L, NewL).
	

insert_single_sum_hex_rec(Max, B, Min, Max, Tail, [hex(Max, B) | Tail]) :- 
	assert(hex(0, Max, B, empty)), !.

insert_single_sum_hex_rec(A, B, Min, Max, Tail, L) :-
	A1 is A + 1,
	B1 is B - 1,
	assert(hex(0, A, B, empty)),
	insert_single_sum_hex_rec(A1, B1, Min, Max, [hex(A, B) | Tail], L).



insert_all_hex(L) :-
	retractall(hex(_, X, Y, State)),
	retractall(selected_hex(_, _)),
	board_size(Size),
	RowsNum is 2 * Size - 1,
	insert_all_hex_rec(1, RowsNum, [], L).
		

insert_all_hex_rec(Row, Row, L, NewL) :-
	board_size(Size),
	Sum is 2 * Row + Size - 1,
	calc_min(Row, Size, Min),
	calc_max(Row, Size, Max),
	insert_single_sum_hex(Sum, Min, Max, L, NewL),
	!.

insert_all_hex_rec(Row, RowsNum, L, NewL) :-
	board_size(Size),
	Sum is 2 * Row + Size - 1,
	calc_min(Row, Size, Min),
	calc_max(Row, Size, Max),
	insert_single_sum_hex(Sum, Min, Max, L, NewL1),
	NextRow is Row + 1,
	insert_all_hex_rec(NextRow, RowsNum, NewL1, NewL).


calc_min(Row, BoardSize, 1) :-
	Row =< BoardSize, !.
calc_min(Row, BoardSize, Min) :-
	Min is Row - BoardSize + 1.


calc_max(Row, BoardSize, Max) :-
	Row =< BoardSize, !,
	Max is Row + BoardSize - 1.

calc_max(Row, BoardSize, Max) :-
	Max is 2 * BoardSize - 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% if chose to close, close and use default settings.
%hello_handler( hello, msg_close , Data, close ):-
%  assert(board_size(8)),
%  assert(first_player(human)),
%  assert(search_depth(3)),
%  true.


%hex_handler( (hex,2), msg_button, _) :-
%	msgbox(`The Result`, '123', 0, _).
	
%  show_settings.

% if pressed 'Use Default Settings' close window.
hello_handler( (hello,3), msg_button, _, close):-
  assert(board_size(8)),
  assert(first_player(human)),
  assert(search_depth(3)),
  true.


%%%%%%%%%%%%%%%%%%%
