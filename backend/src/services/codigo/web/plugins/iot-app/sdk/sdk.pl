:- module(sdk, [activar_sdk/0, thing/4, sensor/7, regla/5, imprimir_kit/0, imprimir_kit_json/1, imprimir_reglas/1, imprimir_reglas_json/1]).
:- use_module('./modulos/ui/menu.pl', [menu/0, submenu_opcion/2]).
:- use_module('./modulos/dominio/thing.pl', [inicializar_things/0, imprimir_thing/1, imprimir_kit/1, imprimir_kit_json/1]).
:- use_module('./modulos/dominio/reglas.pl', [inicializar_reglas/0, imprimir_reglas/2, imprimir_reglas_json/1]).

% ******************** SDK *****************************
% * Funcionalidad: a) dominio, b) reglas, c) simulador *
% ******************** SDK *****************************

% thing(IdThing, Nombre, Descripción, [ListaSensores])
:- dynamic thing/4.

% sensor(Id, Nombre, Descripción, Unidad, ValorActual, ValorConsigna, ValorIncremento).
:- dynamic sensor/7.

% regla(IdRegla, Criterio, Accion, Descripcion, Activada)
:- dynamic regla/5.

imprimir_reglas(StringResult) :-
    findall(
        regla(IdRegla, Criterio, Accion, Descripcion, Activada),
        (
            regla(IdRegla, Criterio, Accion, Descripcion, Activada)
        ),
        Reglas
    ),
    length(Reglas, LongitudR),
    format(atom(Header), '    - Reglas: ~w~n', [LongitudR]),
    % Call imprimir_reglas from reglas.pl to build the result string
    imprimir_reglas(Reglas, Body),
    atom_concat(Header, Body, StringResult).

% Nueva función para devolver reglas como JSON
imprimir_reglas_json(JsonString) :-
    % Llama a la función imprimir_reglas_json del módulo reglas.pl
    imprimir_reglas_json(JsonString).

imprimir_kit(StringResult) :-
    findall(
        thing(Id, Nombre, Descripcion, ListaSensores),
        thing(Id, Nombre, Descripcion, ListaSensores),
        Things
    ),
    maplist(imprimir_thing, Things, ListaStrings),
    atomic_list_concat(ListaStrings, '\n', StringResult).

imprimir_kit_json(JsonString) :-
    % Llama a la función imprimir_kit_json del módulo thing.pl
    imprimir_kit_json(JsonString).

% Instancia el SDK y lanza la interfaz UI
activar_sdk:- format('[Modulo: SDK] ~n', []),
    inicializar_things,
    inicializar_reglas,
    menu.
