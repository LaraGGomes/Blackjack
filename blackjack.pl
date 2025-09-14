% ==========================
% Blackjack em Prolog
% ==========================
:- use_module(library(random)).

% ---------------------------
% Delay na escrita
% ---------------------------
delay_put_char(X) :-
    put_char(X),
    flush_output, 
    sleep(0.025).

delayText(String) :-
    string_chars(String, Chars),
    maplist(delay_put_char, Chars).

% ---------------------------
% Baralho
% ---------------------------
valor(2,2). valor(3,3). valor(4,4). valor(5,5). valor(6,6).
valor(7,7). valor(8,8). valor(9,9). valor(10,10).
valor(j,10). valor(q,10). valor(k,10). valor(a,11).

% Naipes em unicode
%      copas           espadas           ouros             paus
naipe('\u2665'). naipe('\u2660'). naipe('\u2666'). naipe('\u2663').

% Gera baralho completo
baralho(Deck) :-
    findall(carta(Valor, Pontos, Naipe), (naipe(Naipe), valor(Valor, Pontos)), Deck).

% Embaralhar o baralho
embaralhar(Deck, DeckEmbaralhado) :-
    random_permutation(Deck, DeckEmbaralhado).

% ---------------------------
% Valor da mão (corrigido para múltiplos Ases)
% ---------------------------
valor_mao(Mao, ValorFinal) :-
    % Conta Ases como 11 inicialmente
    findall(V, (member(carta(Vale, P, _), Mao), (Vale = a -> V = 11 ; V = P)), Valores),
    sum_list(Valores, Soma),
    conta_as(Mao, NumAses),
    ajusta_ases(Soma, NumAses, ValorFinal).

% Conta quantos Ases existem
conta_as([], 0).
conta_as([carta(a,_,_)|Resto], N) :- conta_as(Resto, N1), N is N1 + 1.
conta_as([_|Resto], N) :- conta_as(Resto, N).

% Ajusta valor dos Ases para não estourar 21
ajusta_ases(Soma, 0, Soma).
ajusta_ases(Soma, NumAses, ValorFinal) :-
    Soma > 21, NumAses > 0,
    Soma1 is Soma - 10,
    NumAses1 is NumAses - 1,
    ajusta_ases(Soma1, NumAses1, ValorFinal).
ajusta_ases(Soma, _, Soma).

% ---------------------------
% Mostrar cartas
% ---------------------------
mostra_carta(carta(a,_,Naipe)) :- delayText('A['), delayText(Naipe), delayText('] ').
mostra_carta(carta(Valor,_,Naipe)) :- delayText(Valor), delayText('['), delayText(Naipe), delayText('] ').

mostra_mao([]) :- nl.
mostra_mao([Carta|Resto]) :- mostra_carta(Carta), mostra_mao(Resto).

% ---------------------------
% Jogo principal
% ---------------------------
jogar :-
    baralho(Deck),
    embaralhar(Deck, DeckEmbaralhado),
    DeckEmbaralhado = [J1,J2,D1,D2|RestoDeck],

    Jogador = [J1,J2],
    Dealer = [D1,D2],

    write('============================='), nl,
    write('Suas cartas:'), nl,
    mostra_mao(Jogador),
    nl,
    delayText('Carta vis\u00EDvel do dealer:'), nl,
    mostra_carta(D1), nl,
    write('============================='), nl,

    % Verifica Blackjack imediato
    valor_mao(Jogador, ValorJogador),
    (ValorJogador =:= 21 ->
        delayText('Blackjack! Voc\u00EA venceu imediatamente!'), nl,
        rejogar
    ;
        turno_jogador(Jogador, RestoDeck, NovoDeck, MaoJogador),
        valor_mao(MaoJogador, ValorJogadorFinal),
        (ValorJogadorFinal > 21 ->
            delayText('Voc\u00EA estourou! Dealer vence.'), nl,
            rejogar
        ;
            turno_dealer(Dealer, NovoDeck, MaoDealer),
            valor_mao(MaoDealer, ValorDealer),

            nl, delayText('---------- Resultado ----------'), nl,
            delayText('Sua m\u00E3o final: '), mostra_mao(MaoJogador),
            delayText('Valor: '), delayText(ValorJogadorFinal), nl, nl,
            delayText('M\u00E3o final do dealer: '), mostra_mao(MaoDealer),
            delayText('Valor: '), delayText(ValorDealer), nl, nl,
            resultado(ValorJogadorFinal, ValorDealer)
        )
    ).

% ---------------------------
% Turno do jogador
% ---------------------------
turno_jogador(Mao, Deck, DeckFinal, MaoFinal) :-
    valor_mao(Mao, Valor),
    (Valor >= 21 ->
        DeckFinal = Deck,
        MaoFinal = Mao
    ;   
        delayText('Total da sua m\u00E3o: '), delayText(Valor), nl,
        nl, delayText('Digite "pedir." para pedir carta ou "parar." para parar: '), read(Acao),
        (Acao == pedir ->
            Deck = [NovaCarta|RestoDeck],
            append(Mao, [NovaCarta], NovaMao),
            write('============================='), nl,
            delayText('Sua m\u00E3o agora:'), nl,
            mostra_mao(NovaMao), nl,
            turno_jogador(NovaMao, RestoDeck, DeckFinal, MaoFinal)
        ;
            Acao == parar ->
            DeckFinal = Deck,
            MaoFinal = Mao
        )
    ).

% ---------------------------
% Turno do dealer
% ---------------------------
turno_dealer(Mao, Deck, MaoFinal) :-
    valor_mao(Mao, Valor),
    (Valor < 17 ->
        Deck = [NovaCarta|RestoDeck],
        append(Mao, [NovaCarta], NovaMao),
        nl, delayText('O dealer pediu! M\u00E3o do Dealer:'), nl,
        mostra_mao(NovaMao), nl,
        turno_dealer(NovaMao, RestoDeck, MaoFinal)
    ;
        MaoFinal = Mao
    ).

% ---------------------------
% Resultado final
% ---------------------------
resultado(ValorJogador, ValorDealer) :-
    ((ValorDealer > 21 -> delayText('Dealer estourou! Voc\u00EA venceu!'), nl
    ; ValorJogador > ValorDealer -> delayText('Voc\u00EA venceu!'), nl
    ; ValorJogador < ValorDealer -> delayText('Dealer venceu!'), nl
    ; ValorJogador =:= ValorDealer -> delayText('Empate!'), nl
    ), rejogar
    ).

% ---------------------------
% Jogar novamente
% ---------------------------
rejogar :-
    nl, delayText('Gostaria de jogar novamente? (sim./nao.): '), read(Acao),
    (Acao == sim ->
        jogar
    ;
        Acao == nao ->
        halt
    ;
        nl, delayText('Resposta inv\u00E1lida.'),
        rejogar
    ).
