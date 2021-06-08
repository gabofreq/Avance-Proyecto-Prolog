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
tokenize(Z) --> "+",  tokenize(Y), {Z = [+ | Y]}.
tokenize(Z) --> "-",  tokenize(Y), {Z = [- | Y]}.
tokenize(Z) --> "*",  tokenize(Y), {Z = [* | Y]}.
tokenize(Z) --> "/",  tokenize(Y), {Z = [/ | Y]}.


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


op1(Opr):- Opr = '+'; Opr = '-'.
op2(Opr):- Opr = '*'; Opr = '/'.
id(X):- atom(X).
inte(X):- integer(X).
floate(X):- float(X).
stringe(X):- string(X).

expr([X|Rest],Rest):-id(X).% la cabeza es una expresion si solo si X es un id
expr0([X|TSAfter],TSAfter, id(X)):- id(X).% hecho
expr0([X|TSAfter],TSAfter, inte(X)):- integer(X).% hecho
expr0([X|TSAfter],TSAfter, floate(X)):- float(X).% hecho
expr0([X|TSAfter],TSAfter, stringe(X)):- string(X).% hecho
expr0(['('|TSBefore],TSAfter,RT):- expr(TSBefore,[')'|TSAfter],RT).%hecho
expr1([Expr0|Rest],TSAfter):-expr0(Expr0).% primera regla recursiva
expr1([Expr1,Op2,Expr0|Rest],TSAfter):-expr1(Expr1),op2(Op2),expr0(Expr0).% segunda regla de recursividad        
expr2([Expr1|TSAfter],TSAfter):-expr1(Expr1).% primera regla recursividad
expr2([Expr2,Op1,Expr1|Rest],TSAfter):-expr2(Expr2),op1(Op1),expr1(Expr1).% segunda regla recursividad


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


chekProgram(FileName):-
	open(FileName,'read',InputStream),
	read_stream_to_codes(InputStream, ProgramString),
	close(InputStream),
	phrase(tokenize(TSBefore), ProgramString),
        write(TSBefore),
	program(TSBefore,TSAfter).