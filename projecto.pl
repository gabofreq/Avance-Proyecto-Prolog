%% Author: Javier Tibau

list([H|T],H,T).

%% Tokenizer code
%Created by Bruno Dufour, Fall 2005
% Append
append([ ],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).


gather(Chars) --> [C],  {alphaNumeric(C)}, gather(Rest), {Chars=[C|Rest]}.

gather([]) --> {true}.
alphaNumeric(C):- 96<C,C<123;
                  64<C, C<91;
                  47<C, C<58.

% - Floats ---------------------------------------------------------------------
digit(D) --> [D], {47 < D, D < 58}.
nzdigit(D) --> [D], {48 < D, D < 58}.

floatlit(F) -->
        nzdigit(D0),
        digits(D1),
        ".",
        nedigits(D2),
        {append([D0|D1], [46], T), append(T, D2, D), name(F, D)}.

nedigits([D|T]) -->
        digit(D), !,
        digits(T).

digits(L) --> nedigits(L).
digits([]) --> [].

% - Strings --------------------------------------------------------------------

quote('"').

gatherString(Chars) --> [C], {C=\=34}, gatherString(Rest), {Chars=[C|Rest]}.
gatherString([]) --> {true}.

stringlit(S) --> "\"", gatherString(Chars), "\"", {string_to_list(S,Chars)}.

% ------------------------------------------------------------------------------

% Tokeinze comparison operators
tokenize(Z) --> "==", tokenize(Y), {Z = [== | Y]}.
tokenize(Z) --> ">=", tokenize(Y), {Z = [>= | Y]}.
tokenize(Z) --> "<=", tokenize(Y), {Z = [<= | Y]}.
tokenize(Z) --> "<>", tokenize(Y), {Z = [<> | Y]}.
tokenize(Z) --> ">",  tokenize(Y), {Z = [> | Y]}.
tokenize(Z) --> "<",  tokenize(Y), {Z = [< | Y]}.



% Tokenize float
tokenize(Result) --> floatlit(F), tokenize(Rest), {Result=[F|Rest]}.
% Tokenize string
tokenize(Result) --> stringlit(S), tokenize(Rest), {Result=[S|Rest]}.
% Tokenize id / int
tokenize(Result) --> gather(Chars),{\+ Chars =[]},tokenize(RestResult), 
                    {name(N,Chars), Result=[N|RestResult]}. 
% Discard whitespace
tokenize(R)-->[C],{C<33},tokenize(R).
% Tokenize special character
tokenize([N|R]) --> [C],{C>32},
                        {name(N,[C])},tokenize(R).
tokenize([])-->[].

% Deficion operadores
op1(+).
op1(-).
op2(*).
op2(/).
op(Opr):- op1(Opr)  |  op2(Opr).

% Relacion id
id(X):- atom(X), \+ op1(X), \+ op2(X). 

% Expresiones
expr0([X|TSAfter],TSAfter):- id(X).% hecho
expr0([X|TSAfter],TSAfter):- integer(X).% hecho
expr0([X|TSAfter],TSAfter):- float(X).% hecho
expr0([X|TSAfter],TSAfter):- string(X).% hecho
expr0(['('|TSBefore],TSAfter):- expr(TSBefore,[')'|TSAfter]).%hecho

expr1(TSBefore,TSAfter):-expr0(TSBefore, Rest).% Primera regla recursiva
expr1(TSBefore,TSAfter):-
	expr0(TSBefore,[Opr|TSAfter]).
	op2(Op2).
	expr0(TSBefore,TSAfter).% Segunda regla de recursividad        

expr2(TSBefore,TSAfter):-expr1(TSBefore,TSAfter).% Primera regla recursividad
expr2(TSBefore,TSAfter):-expr1(TSBefore,[Opr|TSAfter]),op1(Op1),expr(TSBefore,TSAfter).% Segunda regla recursividad

% Programas

program(TSBefore,TSAfter):- statement(TSBefore,TSAfter).
statement(TSBefore,TSAfter):- functionCall(TSBefore,TSAfter).
statement(TSBefore,TSAfter):- assignStatement(TSBefore,TSAfter).
statement(TSBefore,TSAfter):- expr(TSBefore,TSAfter).

assignStatement([V1,'=',V2|Rest],TSAfter):-
        id(V1),
        id(V2),
        expr(Rest,TSAfter).% El resultado es una expresion y devuelve TSAfter

functionCall([Fname,'(',Arg,')'|Rest],Rest):-
        id(Fname),
        id(Arg).

% Llamadas de Programas

chekProgram(FileName):-
	open(FileName,'read',InputStream),
	read_stream_to_codes(InputStream, ProgramString),
	close(InputStream),
	phrase(tokenize(TSBefore), ProgramString),
        write(TSBefore),
	program(TSBefore,[]).
