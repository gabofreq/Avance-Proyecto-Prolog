% Autor: Gabriel Mantilla
% Definiones funciones basicas
reverse([X|Y],Z,W) :- reverse(Y,[X|Z],W).
reverse([],X,X).
member(X,[X|R]).
member(X,[Y|R]):- member(X,R).
list_member(X,[X|_]).
list_member(X,[_|R]) :- list_member(X,R).
list_append(A,T,T) :- list_member(A,T),!.
list_append(A,T,[A|T]).
lookup(X,[X|_],[V|_],V).
lookup(X,[_|R],[_|A],V) :- lookup(X,R,A,V).


% Generador elementos matrices
element(RowN-ColN, Matrix, El) :-
    row(RowN, Matrix, Row),
    nth1(ColN, Row, El).

% Generador elemento simetrico, i.e. where Aij = Aji.
symmetric_element(Matrix, RowN-ColN) :-
    element(RowN-ColN, Matrix, El),
    element(ColN-RowN, Matrix, El).

% Generador indices para filas y columnas.
get_index_pair(N, RowN-ColN) :-
    between(1, N, RowN),
    succ(RowN, RowN1),
    between(RowN1, N, ColN).

% Generador de matrices simetricas
symmetric(Matrix) :-
    length(Matrix, N),
    findall(IndexPair, get_index_pair(N, IndexPair), IndexPairs),
    maplist(symmetric_element(Matrix), IndexPairs).
symmetrical(M) :-
    transpose(M, M).

length_list(N, List) :- length(List, N).

% Generar matriz de ceros
zero_matrix(N, K) :-
    zero_matrix(N, N, K).

zero_matrix(0, _, []) :- !.
zero_matrix(N, M, [K|Ks]) :-
    N1 is N - 1,
    zero_vector(M, K),
    zero_matrix(N1, M, Ks).

zero_vector(0, []) :- !.
zero_vector(M, [0|Ks]) :-
    M1 is M - 1,
    zero_vector(M1, Ks).

% transponer matriz
transpose([A|As], At) :-
    transpose(A, [A|As], At).

transpose([], _, []).
transpose([_|As], Rest, [At|Ats]) :-
    first_column(Rest, At, NewRest),
    transpose(As, NewRest, Ats).

first_column([], [], []).
first_column([[A|As]|Ass], [A|Acc], [As|Rest]) :-
    first_column(Ass, Acc, Rest).

% matriz simetrica
symmetric(A) :- transpose(A, A).

% Ciclo for
for(X , Y , Z):-
    X=<Y,
    X1 is X+Z,
    writeln(X),
    for(X1 , Y , Z).

% pregunta tamaño
size([], 0, _).
size([A|As], Rows, Cols) :-
        valid([A|As]),
        length(A, Cols),
        length([A|As], Rows).

valid([[A|As]|Ass]) :-
    length([A|As], Cols),
    valid(Ass, Cols).

% 
valid([], _).
valid([A|As], Cols) :-
    length(A, Cols),
    valid(As, Cols).

% simulador data frame de R
r_data_frame(RVar, ColSpec, Goal) :-
	must_be(atom, RVar),
   	maplist(arg(1), ColSpec, Names),
   	maplist(arg(2), ColSpec, Vars),
   	Templ =.. [v|Vars],
   	findall(Templ, Goal, Rows),
   	r_data_frame_from_rows(RVar, Rows),
   	colnames(RVar) <- Names.

r_data_frame_to_dicts(DataFrame, Dicts) :-
	Cols <- DataFrame,
	ColNameStrings <- colnames(DataFrame),
	maplist(atom_string, ColNames, ColNameStrings),
	pairs_keys_values(Pairs, ColNames, _),
	dict_pairs(Templ, _, Pairs),
	maplist(dict_cols(Templ, Dicts), ColNames, Cols).

dict_cols(Templ, Dicts, Name, Col) :-
 	maplist(fill_col(Templ, Name), Col, Dicts).

fill_col(_, Name, Value, Dict) :-
	nonvar(Dict), !,
	get_dict(Name, Dict, Value).

fill_col(Templ, Name, Value, Dict) :-
	copy_term(Templ, Dict),
	get_dict(Name, Dict, Value).

r_data_frame_to_rows(DataFrame, Functor, Rows) :-
	Cols <- DataFrame,
	length(Cols, Arity),
	term_cols(Cols, 1, Arity, Functor, Rows).

term_cols([], _, _, _, _).
term_cols([Col|Cols], I, Arity, Functor, Rows) :-
	maplist(term_col(I, Arity, Functor), Col, Rows),
	I2 is I+1,
	term_cols(Cols, I2, Arity, Functor, Rows).

term_col(1, Arity, Functor, Value, Term) :- !,
	functor(Term, Functor, Arity),
	arg(1, Term, Value).

6term_col(I, _, _, Value, Term) :-
	arg(I, Term, Value).

r_data_frame_from_dicts(DataFrame, Rows) :-
	must_be(atom, DataFrame),
	must_be(list, Rows),
	Rows = [Row1|_],
	dict_keys(Row1, Keys),
	dict_col_data(Keys, Rows, ColData),
	compound_name_arguments(Term, 'data.frame', ColData),
	DataFrame <- Term,
	colnames(DataFrame) <- Keys.

dict_col_data([], _, []).
dict_col_data([K|Keys], Rows, [ColI|ColR]) :-
	maplist(get_dict(K), Rows, ColI),
	dict_col_data(Keys, Rows, ColR).

r_data_frame_from_rows(DataFrame, Rows) :-
	must_be(atom, DataFrame),
	must_be(list, Rows),
	Rows = [Row1|_],
	functor(Row1, _, NCols),
	col_data(1, NCols, Rows, ColData),
	append(ColData, [stringsAsFactors = 'FALSE'], ColDataOpts),
	compound_name_arguments(Term, 'data.frame', ColDataOpts),
	DataFrame <- Term.

col_data(I, NCols, Rows, [ColI|ColR]) :-
	I =< NCols, !,
	maplist(arg(I), Rows, ColI),
	I2 is I + 1,
	col_data(I2, NCols, Rows, ColR).
col_data(_, _, _, []).

r_data_frame_colnames(DataFrame, ColNames) :-
	ColNameStrings <- colnames(DataFrame),
	maplist(atom_string, ColNames, ColNameStrings).

r_data_frame_rownames(DataFrame, RowNames) :-
	RowNameStrings <- rownames(DataFrame),
	maplist(atom_string, RowNames, RowNameStrings).
