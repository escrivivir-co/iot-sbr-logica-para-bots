:- module(regla, [inicializar_reglas/0, imprimir_reglas/2, imprimir_reglas_json/1, buscar_reglas/3]).
:- use_module(library(http/json)).
:- use_module('../../sdk.pl', [regla/5, sensor/7]).

% ******************** SDK/MODULOS/REGLAS *****************************
% * Métodos para gestionar reglas *
% ******************** SDK/MODULOS/REGLAS *****************************

inicializar_reglas :-
    format('[Modulo: Reglas] ~n', []).

% Dado un sensor, devuelve una lista de reglas cuya condición
% coincida con el valor actual del sensor
buscar_reglas(IdSensor, Valor, ReglasCoincidentes) :-
    findall(
        regla(IdRegla, Criterio, Accion, Descripcion, Activada),
        (
            regla(IdRegla, Criterio, Accion, Descripcion, Activada),
            Activada,
            call(Criterio, IdSensor, Valor),
            call(Accion, IdRegla, IdSensor, Valor)
        ),
        ReglasCoincidentes
    ).

% imprimir_reglas/2 ya devuelve un string con las reglas formateadas
imprimir_reglas([], "").

imprimir_reglas([regla(Id, _, _, Desc, Activada)|Resto], Result) :-
    format(atom(Line), '        - Regla ID: ~w, Descripción: ~w, (Activada: ~w)~n', [Id, Desc, Activada]),
    imprimir_reglas(Resto, RestLines),
    atom_concat(Line, RestLines, Result).

% Nueva función: Imprimir reglas como JSON
imprimir_reglas_json(JsonString) :-
    % Buscar todas las reglas en el sistema
    findall(
        regla(IdRegla, Criterio, Accion, Descripcion, Activada),
        regla(IdRegla, Criterio, Accion, Descripcion, Activada),
        Reglas
    ),
    % Convertir las reglas en una lista de objetos Prolog con formato de pares clave-valor
    maplist(regla_a_json_objeto, Reglas, ListaJson),
    % Convertir la lista de objetos Prolog en una cadena JSON
    atom_json_term(JsonString, ListaJson, []).

% Regla a un término en formato JSON
regla_a_json_objeto(regla(Id, Criterio, Accion, Descripcion, Activada), 
                    json([id=Id, criterio=Criterio, accion=Accion, descripcion=Descripcion, activada=Activada])).
