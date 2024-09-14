
:- module(sdk, [activar_sdk/0, thing/4, sensor/7, regla/5, imprimir_kit/0,  imprimir_reglas/0]).
:- use_module('./modulos/ui/menu.pl', [menu/0, submenu_opcion/2]).
:- use_module('./modulos/dominio/thing.pl', [inicializar_things/0, imprimir_thing/1]).
:- use_module('./modulos/dominio/reglas.pl', [inicializar_reglas/0, imprimir_reglas/1]).

% ******************** SDK *****************************
% * Funcionalidad: a) dominio, b) reglas, c) simulador *
% ******************** SDK *****************************


% thing(IdThing, Nombre, Descripción, [ListaSensores])
:- dynamic thing/4.

% sensor(Id, Nombre, Descripción, Unidad, ValorActual, ValorConsigna, ValorIncremento).
:- dynamic sensor/7.

% regla(IdRegla, Criterio, Accion, Descripcion, Activada)
:- dynamic regla/5.

imprimir_kit :-
    writeln('- Things del Kit:'),
    forall(
        thing(IdThing, _, _, _),
        (
            imprimir_thing(IdThing)
        )
    ).

imprimir_reglas :-
    findall(
        regla(IdRegla, Criterio, Accion, Descripcion, Activada),
        (
            regla(IdRegla, Criterio, Accion, Descripcion, Activada)
        ),
        Reglas
    ),
    length(Reglas, LongitudR),
    format('    - Reglas: ~w~n', [LongitudR]),
    imprimir_reglas(Reglas).

% Instancia el SDK y lanza la interfaz UI
activar_sdk:- format('[Modulo: SDK] ~n', []),
    inicializar_things,
    inicializar_reglas,
    menu.
