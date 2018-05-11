% --------------------------------------------------------------------------------------
% | Projeto de LP - 2017/2018 - 2Semestre
% | Nome: Rodrigo Borges Pessoa de Sousa
% | Curso: LEIC-A Numero: IST189535
% --------------------------------------------------------------------------------------

% FUNCOES EXTRA (AUXILIARES)
% --------------------------------------------------------------------------------------
% |  lista_ate_Elemento(Lista, Elem, Resultado)
% |  Dado uma lista Lista que contem o elemento Elem, Resultado sera a lista de
% | todos os elementos de Lista ate ao elemento Elem, inclusive.
% --------------------------------------------------------------------------------------
lista_ate_Elemento([Elem|_], Elem, [Elem|[]]).
lista_ate_Elemento([Cabeca|Resto], Elem, [Cabeca|Resto2]) :-
    Cabeca \= Elem,
    lista_ate_Elemento(Resto, Elem, Resto2).

% --------------------------------------------------------------------------------------
% |  coordenada_superior_linha(Posicoes, L)
% |  Dado uma lista Posicoes, de posicoes (coordenada linha e coluna), todas estas
% | posicoes estarao na linha L ou superior a esta.
% --------------------------------------------------------------------------------------
coordenada_superior_linha([], _).
coordenada_superior_linha([(X,_)|Resto], L) :-
    X >= L,
    coordenada_superior_linha(Resto, L).

% --------------------------------------------------------------------------------------
% |  valorMaxColuna(Puz, C, Max)
% |  Dado uma puzzle Puz, um inteiro C correspondente a coluna em questao, Max sera
% | o valor maximo de posicoes preenchidas na coluna em questao.
% --------------------------------------------------------------------------------------
valorMaxColuna(Puz, C, Max) :-
    Puz = [_, _, LimColunas],
    nth1(C, LimColunas, Max).

% --------------------------------------------------------------------------------------
% |  verifica_parcial_coluna(Posicoes, Coluna, VMax)
% |  Dado uma lista Posicoes que contem varias posicoes do puzzle, um inteiro C 
% | correspondente a coluna em questao, verifica que as Posicoes na Coluna C nao
% | ultrapassam o valor maximo VMax de posicoes permitidas nessa coluna.
% --------------------------------------------------------------------------------------    
verifica_parcial_coluna([], _, VMax) :-
    VMax >= 0 .
verifica_parcial_coluna([(_,C)|Resto], C, VMax) :-
    VMaxTemp is VMax-1,
    verifica_parcial_coluna(Resto, C, VMaxTemp).
verifica_parcial_coluna([(_,Y)|Resto], C, VMax) :-
    Y \= C,
    verifica_parcial_coluna(Resto, C, VMax).

% --------------------------------------------------------------------------------------
% |  verifica_Colunas(Puz, Lista, Dim)
% |  Dado um puzzle Puz, uma lista de posicoes Lista verifica para todas as colunas do
% | puzzle de dimensao Dim que nao ultrapassa o valor maximo de posicoes permitidas.
% --------------------------------------------------------------------------------------    
verifica_Colunas(Puz, Lista, 1) :-
    valorMaxColuna(Puz, 1, VMax),
    verifica_parcial_coluna(Lista, 1, VMax).
verifica_Colunas(Puz, Lista, Dim) :-
    Dim > 1,
    valorMaxColuna(Puz, Dim, VMax),
    verifica_parcial_coluna(Lista, Dim, VMax),
    DimTemp is Dim-1,
    verifica_Colunas(Puz, Lista, DimTemp).

% --------------------------------------------------------------------------------------
% |  combinacao(Tam, Lista, Comb)
% |  Dado uma lista Lista, um inteiro Tam, Comb sera uma lista formada por uma combinacao
% | de elementos de Lista, contendo Tam elementos.
% --------------------------------------------------------------------------------------
combinacao(0, _, []).
combinacao(Tam, [X|Resto], [X|Comb]) :-
    Tam > 0,
    TamTemp is Tam-1,
    combinacao(TamTemp, Resto, Comb).
combinacao(Tam, [_|Resto], Comb) :-
    Tam > 0,
    combinacao(Tam, Resto, Comb).

% --------------------------------------------------------------------------------------
% |  propaga_Lista(Puz, Posicoes, PropagaL)
% |  Dado um puzzle Puz, uma lista de posicoes Posicoes, PropagaL sera uma lista contendo
% | a propagacao de todas as posicoes na lista Posicoes.
% --------------------------------------------------------------------------------------    
propaga_Lista(_, [], []).
propaga_Lista(Puz, [Pos|Resto], PropagaL) :-
    propaga(Puz, Pos, Propaga),
    append(Propaga, PropagaLTemp, PropagaL),
    propaga_Lista(Puz, Resto, PropagaLTemp).

% --------------------------------------------------------------------------------------
% |  tira_Dim(Puz, Dim)
% |  Dado um puzzle Puz, Dim sera um inteiro correspondente a dimensao de Puz.
% --------------------------------------------------------------------------------------    
tira_Dim(Puz, Dim) :-
    Puz = [_, Linhas, _],
    length(Linhas, Dim).
    
% --------------------------------------------------------------------------------------
% |  lista_ate_Elemento(Lista, Elem, Resultado)
% |  Dado um puzzle Puz, Posicoes_linha uma lista das posicoes da linha, Total o numero
% | total de posicoes a preencher na linha, Ja_Preenchidas a lista  das posicoes ja
% | preenchidas anteriormente.
% |  Possibilidade e a lista ordenada de uma possibilidades para preencher a linha.
% --------------------------------------------------------------------------------------
poss_linha_aux(Puz, Poss_L, Total, Ja_Preenchidas, Possibilidade) :-
    Poss_L = [(L,_)|_],
    tira_Dim(Puz, Dim),
    combinacao(Total, Poss_L, Comb),
    intersection(Poss_L, Ja_Preenchidas, LObrig),
    subset(LObrig, Comb),
    propaga_Lista(Puz, Comb, Propaga),
    sort(Propaga, Possibilidade),
    verifica_parcial(Puz, Ja_Preenchidas, Dim, Possibilidade),
    nao_altera_linhas_anteriores(Possibilidade, L, Ja_Preenchidas),
    intersection(Possibilidade, Poss_L, PropagaL),
    length(PropagaL, Total).

% --------------------------------------------------------------------------------------
% |  cria_Posicoes_linha(Dim, Linha, Coluna, Posicoes)
% |  Dado a dimensao Dim de um puzzle, uma linha Linha desse puzzle, Posicoes sera uma
% | lista de todas as Posicoes de Linha tendo em conta Dim.
% --------------------------------------------------------------------------------------
cria_Posicoes_linha(Dim, Linha, Posicoes) :-  cria_Posicoes_linha(Dim, Linha, 1, Posicoes).
cria_Posicoes_linha(Dim, Linha, Dim, [(Linha, Dim)|[]]).
cria_Posicoes_linha(Dim, Linha, Coluna, [Pos|Resto]) :-
    Coluna =< Dim,
    Pos = (Linha, Coluna),
    ColunaTemp is Coluna + 1,
    cria_Posicoes_linha(Dim, Linha, ColunaTemp, Resto).

% --------------------------------------------------------------------------------------
% |  resolve_rec(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, Possibilidades, Solucao)
% |  Dado um puzzle Puz, Dim a dimensao de Puz, a lista de posicoes previamente preenchidas
% | LimLinhas a lista com as posicoes a serem preenchidas em cada linha, a linha
% | Linha, auxilia a encontrar Solucao que sera a solucao do Puzzle ao percorrer todas
% | as possibilidades geradas para uma determinada linha.
% --------------------------------------------------------------------------------------    
resolve_rec(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, [Possibilidade|[]], Solucao) :-
    append(Ja_Preenchidas, Possibilidade, Ja_Preenchidas_Temp),
    resolve_aux(Puz, Dim, Ja_Preenchidas_Temp, LimLinhas, Linha, Solucao).
resolve_rec(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, [Possibilidade|Resto], Solucao) :-
    append(Ja_Preenchidas, Possibilidade, Ja_Preenchidas_Temp),
    resolve_aux(Puz, Dim, Ja_Preenchidas_Temp, LimLinhas, Linha, Solucao);
    resolve_rec(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, Resto, Solucao).

% --------------------------------------------------------------------------------------
% |  resolve_aux(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, Solucao)
% |  Dado um puzzle Puz, Dim a dimensao de Puz, a lista de posicoes previamente preenchidas
% | LimLinhas a lista com as posicoes a serem preenchidas em cada linha, a linha
% | Linha, auxilia a encontrar Solucao que sera a solucao do Puzzle ao percorrer todas
% | todas as linhas de puzzle.
% --------------------------------------------------------------------------------------    
resolve_aux(_, Dim, Solucao, _, Linha, Solucao) :-
    Linha is Dim+1 .
resolve_aux(Puz, Dim, Ja_Preenchidas, LimLinhas, Linha, Solucao) :-
    cria_Posicoes_linha(Dim, Linha, Posicoes_Linha),
    nth1(Linha, LimLinhas, LimLinha),
    possibilidades_linha(Puz, Posicoes_Linha, LimLinha, Ja_Preenchidas, Possibilidades_L),
    Possibilidades_L \=[],
    LinhaTemp is Linha + 1,
    resolve_rec(Puz, Dim, Ja_Preenchidas, LimLinhas, LinhaTemp, Possibilidades_L, Solucao).
    
% --------------------------------------------------------------------------------------
% |  verifica_final(CMax, Solucao)
% |  Dado uma lista com os limites de posicoes preenchidas em cada coluna, Solucao e uma
% | lista de posicoes que verifica se todas as colunas tem o numero certo de posicoes ocupadas.
% --------------------------------------------------------------------------------------  
verifica_final(CMax, []) :-
    list_to_set(CMax, Temp),
    Temp = [0], !.
verifica_final(CMax, [(_,C)|RestoPos]) :-
    Indice is C-1,
    nth0(Indice, CMax, Max),
    MaxTemp is Max-1,
    length(Antes, Indice),
    append(Antes, [_|Resto], CMax),
    append(Antes, [MaxTemp|Resto], CMaxTemp),
    verifica_final(CMaxTemp, RestoPos).

   
% FUNCOES PEDIDAS
% --------------------------------------------------------------------------------------
% |  propaga(Puz, Pos, Posicoes)
% |  Dado o puzzle Puz, o preenchimento da posicao Pos implica o preenchimento
% | de todas as posicoes da lista ordenada de posicoes Posicoes.
% --------------------------------------------------------------------------------------
propaga([Termometros|_], (X,Y), Posicoes) :-
    member(Termometro, Termometros),
    member((X,Y), Termometro),
    lista_ate_Elemento(Termometro, (X,Y), Posicoes_Temp),
    sort(Posicoes_Temp, Posicoes).

% --------------------------------------------------------------------------------------
% |  nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas)
% |  Dada a lista de posicoes Posicoes, representando uma possibilidade de 
% | preenchimento para a linha L, todas as posicoes desta lista pertencendo a 
% | linhas anteriores a L, pertencem a lista de posicoes Ja_Preenchidas. Como
% | o nome indica, esta lista contem todas as posicoes ja preenchidas nas linhas 
% | anteriores a L.
% --------------------------------------------------------------------------------------
nao_altera_linhas_anteriores(_, 1, _).
nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas) :-
    L > 1,
    subtract(Posicoes, Ja_Preenchidas, PosicoesDif),
    coordenada_superior_linha(PosicoesDif, L).

% --------------------------------------------------------------------------------------
% |  verifica_parcial(Puz, Ja_Preenchidas, Dim, Poss)
% |  Sendo: Puz um puzzle; Ja_Preenchidas uma lista das posicoes ja preenchidas
% | por escolhas anteriores; Dim a dimensao de Puz; Poss a lista de posicoes
% | para uma potencial possibilidade de preencher uma linha.
% |  Significa que Poss nao viola os totais das colunas, que se Poss for escolhida,
% | nenhuma coluna fica com um numero de posicoes preenchidas superior ao seu total.
% --------------------------------------------------------------------------------------
verifica_parcial(Puz, Ja_Preenchidas, Dim, Poss) :-
    union(Ja_Preenchidas, Poss, Uniao),
    sort(Uniao, UniaoSort),
    verifica_Colunas(Puz, UniaoSort, Dim).

% --------------------------------------------------------------------------------------
% |  possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L)
% |  Sendo: Puz um puzzle; Posicoes_linha uma lista das posicoes da linha; Total o 
% | numero total de posicoes a preencher na linha; Ja_Preenchidas e a lista 
% | das posicoes ja preenchidas anteriormente.
% |  Possibilidades_L e a lista ordenada de todas as possibilidades para preencher a linha.
% --------------------------------------------------------------------------------------
possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L) :-
    findall(Possibilidade, poss_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidade), Possibilidades_L_Des),
    sort(Possibilidades_L_Des, Possibilidades_L).

% --------------------------------------------------------------------------------------
% |  resolve(Puz, Solucao)
% |  Sendo: Puz e um puzzle; Solucao uma lista de posicoes,
% | Significa que Solucao e a lista de posicoes a preencher no puzzle Puz
% | de forma a obter uma solucao, estando esta ordenada.
% --------------------------------------------------------------------------------------
resolve(Puz, Solucao) :-
    Puz = [_, LimLinhas, LimColunas],
    tira_Dim(Puz, Dim),
    resolve_aux(Puz, Dim, [], LimLinhas, 1, SolucaoDes),
    sort(SolucaoDes, Solucao),
    verifica_final(LimColunas, Solucao).