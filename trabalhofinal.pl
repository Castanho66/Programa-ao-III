%%%Predicado que faz a codificação recursiva de uma palavra

codifica_rec(_,[],[]).  %recursao final onde a lista de palavras = vazia, pois já codificamos todas as letras para codigos
codifica_rec([(A,B) | Xs], [H|T], [C1 |Rc]) :-  %lista de tuplos, a palavra, código a retornar
    codifica_1letra([(A,B) | Xs], H, C1),
    codifica_rec([(A,B) | Xs], T, Rc).

codifica_1letra([(L,C) | R], L ,C)  :- !.
codifica_1letra([_|R], L, C) :- codifica_1letra(R,L,C).



%%% Predicado que faz a descodificação recursiva de um código

descodifica_rec(_,[],[]).
descodifica_rec([(A,B) | Xs], L, [C1 |Rc]) :- %lista de tuplos, código a descodificar, lista de palavras 
    append(X,Y,L),  %L = lista, Y =  resto, X = valor a codificar primeiro , backtracking com todas as combinações de X, Y, para dar L
    descodifica_1codigo([(A,B) | Xs], X, C1), 
    descodifica_rec([(A,B) | Xs], Y, Rc).

descodifica_1codigo([(L,C) | R], C, L) :- !.
descodifica_1codigo([_|R], C,L) :- descodifica_1codigo(R,C,L).



%%%retorna listas encontras com findall de entre dois tamanhos dados das palavras geradas (vindos do predicado gera_palavras_fixed)

gerar_palavras_final(L,N,M,[]).
gerar_palavras_final(L,N,M,[C|T]) :- 
    N =< M,
    N1 is N+1,
    findall(X, gerar_palavra_fixed(L,N,X), Y), %encontra todas as palavras geradas de tamanho N
    append(Y,[],C), 
    gerar_palavras_final(L,N1,M,T). %chama recursivamente com tamanho incrementado



%%%gera apenas uma lista com todas aquelas que saíram do predicado acima (de todas as palavras geradas por ordem)

gerar_size(L,N,M,Y) :-
    gerar_palavras_final(L,N,M,X),      %predicado que retorna em backtrack uma lista com o findall de todos os elementos possiveis na geração de palavras
    length(X,P1),      %tamanho das listas que vêm em backtrack
    my_flatten(X,Y),   %vai juntar as listas que vêm do backtrack x -> retorna findall de todas as iterações das gerações
    P1 = M.            %de palavras



%%%pega na lista de cima, e vai retornando em backtrack todas essas palavras

gera_backtrack(L,N,M,Y) :-
    gerar_size(L,N,M,X),
    member(Y,X). %Para gerar combinações possíveis entre X e Y em backtracking



my_flatten([], []). %juntar as listas , recebe uma lista de listas e retorna apenas uma lista (concatena)
my_flatten([A|B],L) :- is_list(A), my_flatten(B,B1), !, append(A,B1,L). 
my_flatten([A|B],[A|B1]) :- my_flatten(B,B1).



%%%– gera todas as palavras com N tamanho de uma lista de letras

gerar_palavra_fixed(L, 0, []). %gera primeiro todas as palavras com tamanho 1, 2, 3, 4...Consoante o tamanho dado
gerar_palavra_fixed(L,N, [C|W]) :- %N tamanho dado
    N > 0,
    N1 is N-1,
    member(C,L),
    gerar_palavra_fixed(L, N1, W).


%%% Predicado que retorna a lista de letras existentes no dicionário pretendido
%%% Irá ser usado para gerar todas as palavras com as respetivas letras
%%% Exemplo 1 do enunciado vai retornar -> X = [a,c,j,l,p,s,v] // Exemplo 2 do enunciado vai retornar -> X = [a,b,c,f,j,l,r]

prefixo([],[]).
prefixo([(X,_)|T], [X|T2]):- prefixo(T,T2).



%%% Predicado que concatena listas, que por sua vez estejam dentro de uma lista
%%% Exemplo -> [[0,1,0],[1,1]] = [0,1,0,1,1]

juntar_listas([], []) :- !.
juntar_listas([L|Ls], FlatL) :-
    !,
    juntar_listas(L, NewL),
    juntar_listas(Ls, NewLs),
    append(NewL, NewLs, FlatL).
juntar_listas(L, [L]).


%%% Predicado ambiguo = recebe como output o dicionário, e retorna o código ambiguo (P3), e as duas palavras que o codificam (P1 e P4)
%%% Usa todos os predicados anteriores e vai vendo em backtracking todos os códigos ambíguos 

ambiguo(L,P4,P3,P1) :-
    prefixo(L,P),    %retorna uma lista com as letras todas do dicionario [a,c,j,l,p,s,v]
    gera_backtrack(P,1,4,P1), %gera todas as palavras entre o tamanho 1 e 4 por ordem de tamanho
    codifica_rec(L,P1,P2),
    juntar_listas(P2,P3),   %p3 mostra os valores de uma palavra codifica apenas numa lista [[0,1,0],[0,1,0]]  = [0,1,0,0,1,0] 
    length(P3,P5),     
    descodifica_rec(L,P3,P4),  
    P1 \= P4, % P1 e P4 têm de ser logicamente diferentes -> a palavra codifica inicialmente, tem um código que pode ser igual para outra palavra também
    P5 == 2,
    !.
    %P5 == 12 %Para o segundo exemplo

%%%Para testar

%Exemplo 1 do enunciado
%ambiguo([(a, [0,1,0]),(c, [0,1]),(j, [0,0,1]),(l, [1,0]),(p, [0]),(s, [1]),(v, [1,0,1])], M, T1, T2)

%Exemplo2 do enunciado
%ambiguo([(a, [0,1,1,0]),(b, [0,1,1,1,1,1]),(c, [1,1,0,0,1,1,1,1]),(f, [1,0,1,1,1,0]),(j, [0,1,0]),(l, [0,1,0,0]),(r, [0,1,1,1,0])], M, T1, T2)