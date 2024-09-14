:- module(app, [do_init/2, do_start/2, do_increase_loop/2, do_decrease_loop/2, do_pause/2, do_resume/2, do_stop/2, get_current_state/1]).

:- dynamic(current_state/1).

% Set the initial state when the module is loaded
:- initialization(asserta(current_state(initialize))).

% Initialize the machine
do_init(Requester, Result) :-
    retractall(current_state(_)),
    asserta(current_state(initialize)),
    format(atom(Result), 'Máquina inicializada por ~w.', [Requester]).

% Start the machine (from initialize or paused)
do_start(Requester, Result) :-
    current_state(State),
    (   (State \== started)
    ->  retractall(current_state(_)),
        asserta(current_state(started)),
        format(atom(Result), 'Maquina arrancada por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede arrancar desde el estado ~w.', [State])
    ).

% Increase the machine's loop (from started)
do_increase_loop(Requester, Result) :-
    current_state(State),
    (   State = started
    ->  retractall(current_state(_)),
        asserta(current_state(started)),
        format(atom(Result), 'Bucle incrementado por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede incrementar el bucle desde el estado ~w.', [State])
    ).

% Decrease the machine's loop (from increased)
do_decrease_loop(Requester, Result) :-
    current_state(State),
    (   State = started
    ->  retractall(current_state(_)),
        asserta(current_state(started)),
        format(atom(Result), 'Bucle decrementado por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede decrementar el bucle desde el estado ~w.', [State])
    ).

% Pause the machine (from started or increased)
do_pause(Requester, Result) :-
    current_state(State),
    (   State = started
    ->  retractall(current_state(_)),
        asserta(current_state(paused)),
        format(atom(Result), 'Máquina pausada por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede pausar desde el estado ~w.', [State])
    ).

do_resume(Requester, Result) :-
    current_state(State),
    (   State = paused
    ->  retractall(current_state(_)),
        asserta(current_state(started)),
        format(atom(Result), 'Maquina resumida por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede resumir el bucle desde el estado ~w.', [State])
    ).

% Stop the machine (from started, increased, paused, or decreased)
do_stop(Requester, Result) :-
    current_state(State),
    (   (State = started ; State=paused)
    ->  retractall(current_state(_)),
        asserta(current_state(stopped)),
        format(atom(Result), 'Máquina detenida por ~w.', [Requester])
    ;   format(atom(Result), 'No se puede detener desde el estado ~w.', [State])
    ).

% Get the current state of the machine
get_current_state(State) :-
    current_state(State).
