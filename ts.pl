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

removeLast([_], []).
removeLast([X|Xs], [X|WithoutLast]) :- 
    removeLast(Xs, WithoutLast).

getActualSymbol([_, Symbol|_], Symbol).

% funkce vrati nasledujici symbol
getSymbol([], ' ').
getSymbol([H|_], H).

getActualStateAndSymbol([H|T], State, Symbol) :-
	char_type(H, upper),
		(
			getSymbol(T, Symbol),
			State = H
		)
	;
	getActualStateAndSymbol(T, State, Symbol).

getRule([H|T], State, Symbol, NextState, Action) :-
	[StateRule, SymbolRule, NewState, NewSymbol] = H,
	(
		StateRule == State,
		SymbolRule == Symbol,
		NextState = NewState,
		Action = NewSymbol
		;
		getRule(T,State,Symbol,NextState,Action)
	).

shiftLeft([H1,H2,H3|T], State, Symbol, Action, NextState, NewTape) :-
	(
		H2 == State,
		%% writeln([H1,H2|T]),
		writeln("shiftleft"),
		append([NextState, H1, H3], T, NewTape),
		writeln(NewTape)
		;
		%% writeln("somtu"),
		shiftRight([H2,H3|T], State, Symbol, Action, NextState, R),
		NewTape = [H1|R]
	)
.

shiftRight([H1,H2|T], State, Symbol, Action, NextState, NewTape) :-
	(
		H1 == State,
		(
			T == [],
			(
				writeln("konic pasky"),
				NewTape = [NextState, H2],
				fail
			);
			append([H2, NextState], T, NewTape)
		)
		%% writeln([H1,H2|T]),
		;
		writeln("somtu"),
		shiftRight([H2|T], State, Symbol, Action, NextState, R),
		NewTape = [H1|R]
	)
.

writeSymbol([H1,H2|T], State, Symbol, Action, NextState, NewTape) :-
	(	
		H1 == State,
		%% writeln(NewTape),
		%% writeln(State),
		%% writeln(NextState),
		%% writeln(Action),
		%% writeln(Action),
		NewTapeTmp = [NextState, Action],
		append(NewTapeTmp, T, NewTape)
		%% writeln(NewTape)
               %% append([NextState], [Symbol], NewTmp),
               %% append(NewTmp, MoreTape, NewTape)
		;
		writeSymbol([H2|T], State, Symbol, Action, NextState, R),

		NewTape = [H1|R]
	).

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

runTS(_,_,_,0).
runTS(Tape,Rules,Output, Cycles) :-
	getActualStateAndSymbol(Tape, State, Symbol),
	(
		State == 'F',!
		;
		getRule(Rules, State, Symbol, NextState, Action),
		getNextAction(Action, Operation),
		call(Operation, Tape, State, Symbol, Action, NextState, NewTape),
		writeln(NewTape),
		C is Cycles - 1,
		runTS(NewTape, Rules, R, C),
		Output = [NewTape|R]
	).

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		last(S,InputTape),
		removeLast(S, Rules),
		flatten(InputTape, X),
		append(['S'],X,Tape),
		maplist(flatten, Rules, RulesFlattened),
		%% write_lines2(RulesFlattened),
		%% write_lines2(Tape),

		%% writeln(Tape),
		runTS(Tape, RulesFlattened, Output, 20),
		writeln(Output),

		halt.




/* nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].


/* vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")




/* rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
split_line2([],[[]]) :- !.
%pridat novy seznam:
split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


/* pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines2([],[]).
split_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).





/* nacte N radku vstupu, zpracuje, vypise */
start2(N) :-
		prompt(_, ''),
		read_lines2(LL, N),
		split_lines2(LL,S),
		write_lines2(S).





/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).



/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
cislo(N,X) :- cislo(N,0,X).
cislo([],F,F).
cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
cislo([],F,F,_).
cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).

% existuje knihovni predikat number_chars(?Number, ?CharList)
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).
