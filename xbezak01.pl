/** FLP 2018
Program: Touringov stroj
autor: Adam Bezak, xbezak01@stud.fit.vutbr.cz
*/

%%%%%%%%%%%%%%% Pomocne funkcie z cviceni %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

%%%%%%%%%%%%%%% Moje funkcie %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% vypise list charov ako string
writeListInStringFormat([]) :- write('\n').
writeListInStringFormat([H|T]) :- write(H),writeListInStringFormat(T).

% vypise list listov charov ako viacero stringov pod seba
writeListsInStringFormat([]).
writeListsInStringFormat([H|T]) :- writeListInStringFormat(H),writeListsInStringFormat(T).

% odstrani posledny element z listu
removeLast([_], []).
removeLast([X|Xs], [X|WithoutLast]) :- 
    removeLast(Xs, WithoutLast).

% vrati prvy element z listu
getFirst([], First) :- First = ' '.
getFirst([H|_], First) :- First = H.

% vrati aktualny stav a symbol na zaklade konfiguracie TS
getActualStateAndSymbol([H|T], State, Symbol) :-
	char_type(H, alpha),
	char_type(H, upper),
		(
			getFirst(T, Symbol),
			State = H
		)
	;
	getActualStateAndSymbol(T, State, Symbol).

% vrati nasledujuci stav na zaklade aktualneho stavu a symbolu
getRule([H|T], State, Symbol, NextState, Action) :-
	[StateRule, SymbolRule, NewState, NewSymbol] = H,
	(
		StateRule == State,
		SymbolRule == Symbol,
		NextState = NewState,
		Action = NewSymbol
		;
		getRule(T,State,Symbol,NextState,Action)
	)
.

% posun na paske vlavo
shiftLeft([H1,H2,H3|T], State, Symbol, Action, NextState, NewTape) :-
	(
		H1 == State, % ak sa nachadzame na zaciatku pasky tak fail
		(
			fail
		);
		H2 == State,
		append([NextState, H1, H3], T, NewTape)
		;
		shiftLeft([H2,H3|T], State, Symbol, Action, NextState, R),
		NewTape = [H1|R]
	)
.

% posun na paske vpravo
shiftRight([H1,H2|T], State, Symbol, Action, NextState, NewTape) :-
	(
		H1 == State,
		(
			T == [], % ak sa nachadzame na konci pasky tak fail
			(
				NewTape = [NextState, H2],
				fail
			);
			append([H2, NextState], T, NewTape)
		)
		;
		shiftRight([H2|T], State, Symbol, Action, NextState, R),
		NewTape = [H1|R]
	)
.

% nahrada symbolu na paske
writeSymbol([H1,H2|T], State, Symbol, Action, NextState, NewTape) :-
	(	
		H1 == State,
		NewTapeTmp = [NextState, Action],
		append(NewTapeTmp, T, NewTape)
		;
		writeSymbol([H2|T], State, Symbol, Action, NextState, R),

		NewTape = [H1|R]
	).

% vrati odkaz na operaciu na zaklade aktualnej Action
getNextAction(Action, Operation) :-
	Action == 'L', 
	(
		Operation =	shiftLeft
	) 
	;
	Action == 'R',
	(
		Operation =	shiftRight
	)
	;
	Action \= 'L', Action \= 'R',
	(
		Operation = writeSymbol
	)
.

% simulacia NTS
runTS(Tape,Rules,Output) :-
	getActualStateAndSymbol(Tape, State, Symbol),
	(
		State == 'F',!
		;
		getRule(Rules, State, Symbol, NextState, Action), 
		getNextAction(Action, Operation),
		call(Operation, Tape, State, Symbol, Action, NextState, NewTape),
		runTS(NewTape, Rules, R),
		Output = [NewTape|R]
	).

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		% Ziskanie vstupnej pasky
		last(S,InputTape),
		% Ziskanie pravidiel
		removeLast(S, Rules),
		% Prevod z [[]] na []
		flatten(InputTape, X),
		% Pociatocny stav na zaciatok pasky
		append(['S'],X,Tape),
		% Prevod z [[]] na []
		maplist(flatten, Rules, RulesFlattened),
		% Vypis pociatocej konfiguracie
		writeListInStringFormat(Tape),
		% Beh TS
		runTS(Tape, RulesFlattened, Output),
		% Vypis prechodu konfiguracii
		writeListsInStringFormat(Output),

		halt.