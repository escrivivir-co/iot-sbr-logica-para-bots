:- module(simulador, [iniciar_simulador_temporizador/0, parar/0]).
:- use_module('../ui/menu.pl', [submenu/1]).
:- use_module('../dominio/thing.pl', [simulador_thing/1, imprimir_thing/1, imprimir_ficha_thing/1]).

:- use_module(library(thread)).
:- use_module(library(random)).

% ******************** SDK/SIMULADOR *****************************
% * Funcionalidad: a) Ciclo Vida simulación b) Manejador de reglas *
% ******************** SDK/SIMULADOR *****************************

% Variable global para controlar el estado del temporizador
:- dynamic temporizador_activo/1.
temporizador_activo(true).

% Ciclo: para cada Thing registrada en el sistema,
% llama a la transición de estado de sus sensores
:- dynamic estado_contador_programa/1.
estado_contador_programa(0).

contadorDePrograma :-
    estado_contador_programa(Valor),
    ActualizarValor is Valor + 1,
    retractall(estado_contador_programa(_)),
    assert(estado_contador_programa(ActualizarValor)),
    % shell('clear'),
    format('CP con ~w~n', [ActualizarValor]),
    forall(
        thing(IdThing, _, _, _),
        (
            simulador_thing(IdThing),
            imprimir_ficha_thing(IdThing)
        )
    ).

% Función para iniciar el temporizador. Crea 2 threads independientes
%   - Thread de estado: ejecuta el pulso de transición a todos los sensores del mundo
%   - Thread para UI (parada): escucha la señal de parada por parte del usuario
iniciar_simulador_temporizador :-
    writeln('       - Arrancando temporizador de la simulación.'),
    retractall(temporizador_activo(_)),
    assert(temporizador_activo(true)),
    thread_create(ciclo_reloj, _, [detached(true)]),
    thread_create(escuchar_tecla, _, [detached(true)]).

% Ciclo de repetición, a cada paso activa el contador de programa
ciclo_reloj :-
    repeat,
    (
        temporizador_activo(true)
    ->
        writeln('       - Aplicando incremento temporizador.'),
        contadorDePrograma(),
        sleep(1),
        fail
    ; !
    ).

parar :-
    writeln('Iniciar rutina parar.'),
    retractall(temporizador_activo(_)),  % Desactiva el temporizador
    writeln('Temporizador detenido.'),
    submenu(2).
parar(_) :- parar.

% Escucha la tecla del usuario para detener el temporizador
escuchar_tecla :-
    writeln('Pulse cualquier tecla para detener.'),
    read(Tecla),  % Espera una entrada de tecla
    parar(Tecla).

