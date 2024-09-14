:- module(menu, [menu/0, submenu/1, submenu_opcion/2]).
:- use_module('../simulador/simulador.pl', [iniciar_simulador_temporizador/0]).
:- use_module('../../sdk.pl', [imprimir_kit/0, imprimir_reglas/1]).

% ******************** SDK/UI/MENU *****************************
% * Ejecutar prolog? menu. *
% ******************** SDK/UI/MENU *****************************

% Opciones del menú principal
opcion(0, 'Arrancar interfaz', arrancar_interfaz).
opcion(1, 'Arrancar simulador', arrancar_simulador).

% Submenú para la opción 0 - Interfaz
submenu(0, 0, 'Dominio', interfaz_imprimir_kit).
submenu(0, 1, 'Reglas', interfaz_imprimir_reglas).

% Submenú para la opción 1 - Simulador
submenu(1, 0, 'Simular desde el principio', simular_inicio).
submenu(1, 1, 'Simular desde punto de restauración [No implementado]', simular_desde_checkpoint).

% Activadores de pantallas
arrancar_interfaz :- writeln('Interfaz iniciada.').
interfaz_imprimir_kit :- imprimir_kit, manejar_opcion(0).
interfaz_imprimir_reglas :- imprimir_reglas(Result), manejar_opcion(0).
arrancar_simulador :- writeln('Simulador iniciado.').
simular_inicio :- writeln('Simulación iniciada desde el principio.'),
    iniciar_simulador_temporizador.
simular_desde_checkpoint :-
    writeln('Introduzca el número de checkpoint:'),
    read(CheckPoint),
    writeln(['Simulando desde el checkpoint ', CheckPoint]).


% Instancia el menú
menu :-
    writeln('Bienvenido al sistema, seleccione una opción:'),
    forall(opcion(Tecla, Descripcion, _),
           (format(' - ~w: ~w~n', [Tecla, Descripcion]))),
    writeln('3: Salir'),
    read(Choice),
    manejar_opcion(Choice).

manejar_opcion(3) :- writeln('Saliendo del programa...').
manejar_opcion(Choice) :-
    opcion(Choice, _, Accion),
    !,
    call(Accion),
    submenu(Choice).

manejar_opcion(_) :-
    writeln('Opción no reconocida, por favor intente de nuevo.'),
    menu.

submenu(Choice) :-
    forall(submenu(Choice, Subkey, Subdesc, _),
           format(' - ~w: ~w~n', [Subkey, Subdesc])),
    writeln('99: Salir'),
    read(SubChoice),
    submenu_opcion(Choice, SubChoice).

submenu_opcion(_, 99) :- writeln('Saliendo del submenu...'), menu.
submenu_opcion(MainChoice, SubChoice) :-
    submenu(MainChoice, SubChoice, _, Action),
    !,
    call(Action).

submenu_opcion(_, _) :-
    writeln('Opción no válida.'),
    menu.
