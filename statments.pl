% Autor: Gabriel Mantilla
% Definiones funciones basicas
reverse([X|Y],Z,W) :- reverse(Y,[X|Z],W).
reverse([],X,X).
member(X,[X|R]).
member(X,[Y|R]):- member(X,R).
lookup(X,[X|_],[V|_],V).
lookup(X,[_|Vars],[_|A],V) :- lookup(X,Vars,A,V).


% Definicion de Vectores por predicados
vector_type(double(_List), 2).
vector_type(float(_List), 3).
vector_type(integer(_List), 4).
vector_type(integer64(_List), 5).
vector_type(integer32(_List), 6).
vector_type(unsigned(_List), 7).
vector_type(codes(_List), 8).
vector_type(atom(_List), 9).
vector_type(string(_List), 10).

vector(Type, B):-
    vector_type(Type, Tag),
    Proto = protobuf([ repeated(Tag, Type) ]),
    protobuf_message(Proto, B).

% Definicion de matrices por predicados
row(M, N, Row) :-
    nth1(N, M, Row).

column(M, N, Col) :-
    transpose(M, MT),
    row(MT, N, Col).

symmetrical(M) :-
    transpose(M, M).

transpose([[]|_], []) :- !.
transpose([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transpose([Is|NRs], MT).

first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    first_column(Rs, Col, Rest).

