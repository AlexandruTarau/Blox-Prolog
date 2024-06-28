:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state(([], [], [])).

% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile((BPs, Tiles, SWs), Pos, (BPs, [Tile | Tiles], SWs)) :- Tile = (Pos, tile).

% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(S, _, S) :- true.

% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target((BPs, Tiles, SWs), Pos, (BPs, [Tile | Tiles], SWs)) :- Tile = (Pos, target).

% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile((BPs, Tiles, SWs), Pos, (BPs, [Tile | Tiles], SWs)) :- Tile = (Pos, fragile).

% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial((_, Tiles, SWs), Pos, ([Pos], [Tile | Tiles], SWs)) :- Tile = (Pos, tile).

% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos(([], _, _), empty).
get_b_pos(([Pos], _, _), Pos).
get_b_pos((Positions, _, _), Positions).

% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds((_, [], _), 0, 0, 0, 0).
get_bounds((_, [((X, Y), _)], _), X1, X1, Y1, Y1) :- X1 is X, Y1 is Y.
get_bounds((_, [((X, Y), _) | Rest], _), Xmin, Xmax, Ymin, Ymax) :- get_bounds((_, Rest, _), Xmin1, Xmax1, Ymin1, Ymax1),
                                                                 min(Xmin1, X, Xmin), max(Xmax1, X, Xmax), min(Ymin1, Y, Ymin), max(Ymax1, Y, Ymax).

% max/3
% max(+X, +Y, -Z).
max(X, Y, Z) :- X > Y, Z = X; Z = Y.

% min/3
% min(+X, +Y, -Z).
min(X, Y, Z) :- X < Y, Z = X; Z = Y.

% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell((_, [], _), _, _) :- false.

get_cell((_, [((X1, Y1), T) | Rest1], _), (X, Y), What) :-
        X =:= X1, Y =:= Y1, What = T;
        get_cell((_, Rest1, _), (X, Y), What).

% bridge/4
% bridge(+Tiles, +Func, +Positions, -TilesNext)
bridge(Tiles, uponly, Positions, TilesNext) :-
    findall((Pos, tile), member(Pos, Positions), Bridge),
    (Bridge = [First | _], member(First, Tiles) -> NewBridge = []; NewBridge = Bridge),
    append(NewBridge, Tiles, TilesNext).

bridge(Tiles, dnonly, Positions, TilesNext) :-
    findall((Pos, tile), member(Pos, Positions), Bridge),
    (Bridge = [First | _], member(First, Tiles) -> NewBridge = Bridge; NewBridge = []),
    remove_tiles(NewBridge, Tiles, TilesNext).

bridge(Tiles, switch, Positions, TilesNext) :-
    (bridge(Tiles, uponly, Positions, TempTiles), Tiles \= TempTiles, TilesNext = TempTiles;
    bridge(Tiles, dnonly, Positions, TilesNext)).

% remove/3
% remove(+Elem, +Lista, -ListaNoua)
remove(Elem, [Elem | Rest], Rest).
remove(Elem, [Head | Rest], [Head | Left]) :- remove(Elem, Rest, Left).

% remove_tiles/3
% remove_tiles(+Bridge, +Tiles, -TilesNext)
remove_tiles([], Tiles, Tiles).
remove_tiles([Tile | Rest], Tiles, TilesNext) :-
    remove(Tile, Tiles, TilesNew),
    remove_tiles(Rest, TilesNew, TilesNext).

% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move(([Pos], Tiles, SWs), Move, ([Pos1, Pos2], TilesNext, SWs)) :-
        neighbor(Pos, Move, Pos1), neighbor2(Pos, Move, Pos2),
        (get_cell((_, Tiles, _), Pos1, _); !, false),
        (get_cell((_, Tiles, _), Pos2, _); !, false),
        (member((Pos1, oswitch, Func, Positions), SWs) -> bridge(Tiles, Func, Positions, TilesNext);
        member((Pos2, oswitch, Func, Positions), SWs) -> bridge(Tiles, Func, Positions, TilesNext);
        TilesNext = Tiles).

move(([Pos1, Pos2], Tiles, SWs), Move, ([Pos], TilesNext, SWs)) :-
        (neighbor(Pos1, Move, Pos3), Pos3 = Pos2, neighbor(Pos2, Move, Pos),
        (get_cell((_, Tiles, _), Pos, Type); !, false), Type \= fragile;
        neighbor(Pos2, Move, Pos3), Pos3 = Pos1, neighbor(Pos1, Move, Pos),
        (get_cell((_, Tiles, _), Pos, Type); !, false), Type \= fragile),
        (member((Pos, _, Func, Positions), SWs), bridge(Tiles, Func, Positions, TilesNext); TilesNext = Tiles).

move(([Pos1, Pos2], Tiles, SWs), Move, ([Pos3, Pos4], TilesNext, SWs)) :-
        (neighbor(Pos1, Move, Pos3), Pos3 \= Pos2, neighbor(Pos2, Move, Pos4), Pos4 \= Pos1,
        (get_cell((_, Tiles, _), Pos3, _); !, false),
        (get_cell((_, Tiles, _), Pos4, _); !, false)),
        (member((Pos3, oswitch, Func, Positions), SWs), bridge(Tiles, Func, Positions, TilesNext);
        member((Pos4, oswitch, Func, Positions), SWs), bridge(Tiles, Func, Positions, TilesNext);
        TilesNext = Tiles).

% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(([(X, Y)], Tiles, _)) :- member(((X, Y), target), Tiles).

% pentru etapa 2
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, SOut)
% Switch: o sau x
% Func: switch, uponly sau dnonly
% Position: pozitiile podului
set_switch((BPs, Tiles, SWs), Pos, Switch, Func, Positions,
                (BPs, [(Pos, Switch) | Tiles], [(Pos, Switch, Func, Positions) | SWs])).

% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie sa ducă blocul în
% picioare pe poziția scop (target).
solve((BPs, Tiles, SWs), Moves) :-
        solve_backtrack((BPs, Tiles, SWs), [BPs], Moves).

% solve_backtrack/3
% solve_backtrack(+S, +Visited, -Moves)
solve_backtrack(S, _, _) :-
        is_final(S), !.

solve_backtrack(S, Visited, [Move | Moves]) :-
        findall((Dist, Move, SNext),
                (possible_move(S, Move, SNext),
                SNext = (BPs, _, _),
                \+ check_visited(BPs, Visited),
                goal(S, Goal),
                distance(SNext, Goal, Dist)),
                NewMoves),
        msort(NewMoves, SortedMoves),
        member((_, Move, SNext), SortedMoves),
        SNext = (BPs, _, SWs),
        (\+ check_visited(BPs, Visited) ->
                ((is_switch(SNext) ->
                find_unique_switches(Visited, SWs, UniqueSwitches),
                append(UniqueSwitches, Visited, TempVisited),
                remove_duplicates(TempVisited, NewVisited);
                NewVisited = Visited));
                false),
        solve_backtrack(SNext, [BPs | NewVisited], Moves).


possible_move(S, Move, SNext) :-
        Move = u, move(S, u, SNext);
        Move = d, move(S, d, SNext);
        Move = r, move(S, r, SNext);
        Move = l, move(S, l, SNext).

is_switch(([Pos], _, SWs)) :- member((Pos, _, _, _), SWs).
is_switch(([Pos1, Pos2], _, SWs)) :- member((Pos1, _, _, _), SWs), !; member((Pos2, _, _, _), SWs).

check_visited([Pos], Visited) :- member([Pos], Visited).
check_visited([Pos1, Pos2], Visited) :- member([Pos1, Pos2], Visited); member([Pos2, Pos1], Visited).

distance(([(X1, Y1)], _, _), (X2, Y2), D) :-
        D is abs(X1 - X2) + abs(Y1 - Y2).
distance(([(X1, Y1), (X2, Y2)], _, _), (X3, Y3), D) :-
        D is min(abs(X1 - X3) + abs(Y1 - Y3), abs(X2 - X3) + abs(Y2 - Y3)).

goal((_, Tiles, _), Pos) :- member((Pos, target), Tiles).

remove_duplicates([], []).
remove_duplicates([H | T], [H | R]) :-
    \+ member(H, T),
    remove_duplicates(T, R).
remove_duplicates([H | T], R) :-
    member(H, T),
    remove_duplicates(T, R).

find_unique_switches(Visited, SWs, UniqueSwitches) :-
    findall(Pos, (member(Pos, Visited), is_switch((Pos, _, SWs))), AllSwitches),
    remove_duplicates(AllSwitches, UniqueSwitches).