:- module(regla, [inicializar_reglas/0, imprimir_reglas/1, buscar_reglas/3]).
:- use_module('../../sdk.pl', [regla/5, sensor/7]).

% ******************** SDK/MODULOS/REGLAS *****************************
% * Métodos para gestionar reglas *
% ******************** SDK/MODULOS/REGLAS *****************************

inicializar_reglas :-
    format('[Modulo: Reglas] ~n', []).

% Dado un sensor, devuelve una lista de reglas cuya condicion
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

imprimir_reglas([]).

imprimir_reglas([regla(Id, _, _, Desc, Activada)|Resto]) :-
    format('        - Regla ID: ~w, Descripción: ~w, (Activada: ~w)~n', [Id, Desc, Activada]),
    imprimir_reglas(Resto).

verificar_reglas :-
    format('- Reglas registradas en el sistema ~n', []),
    listing(regla).

