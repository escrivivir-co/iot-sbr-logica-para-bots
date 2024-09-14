:- module(kitThing, [inicializar_things/0, imprimir_thing/1, imprimir_ficha_thing/1, simulador_thing/1, imprimir_kit/0]).
:- use_module('../../sdk.pl', [thing/4, imprimir_kit/0]).
:- use_module('./sensor.pl', [sensor/7, transicion_estado_sensor/1, sensor_valor_actual/2]).
:- use_module('./reglas.pl', [inicializar_reglas/0, imprimir_reglas/1, buscar_reglas/3]).

% ******************** SDK/MODULOS/THING *****************************
% * Funcionalidad: a) set/get b) transicion por consigna *
% ******************** SDK/MODULOS/THING *****************************

%
inicializar_things :-
    format('[Modulo: Things] ~n', []).

%
thing_lista_sensores(Id, ListaSensores) :-
    thing(Id, _, _, ListaSensores).

%
imprimir_sensores([]).
imprimir_sensores([IdSensor|Resto]) :-
    sensor(IdSensor, Nombre, Descripcion, Unidad, ValorActual, ValorConsigna, ValorIncremento),
    format('        - ~w, Nombre: ~w, ~w, Valor: (~w) ~w, consigna: ~w (~w)\n',
        [IdSensor, Nombre, Descripcion, Unidad, ValorActual, ValorConsigna, ValorIncremento]),
    imprimir_sensores(Resto).

%
imprimir_estado([]).
imprimir_estado([IdSensor|Resto]) :-
    sensor(IdSensor, _, _, _, ValorActual, ValorConsigna, ValorIncremento),
    format('        - ~w, Valor: (~w) ~w, consigna: ~w\n',
        [IdSensor, ValorActual, ValorConsigna, ValorIncremento]),
    buscar_reglas(IdSensor, ValorActual, Reglas),
    imprimir_reglas(Reglas),
    imprimir_estado(Resto).

%
imprimir_thing(Id) :-
    thing(Id, Nombre, Descripcion, ListaSensores),
    format('    - Thing: (~w),  ~w\n', [Nombre, Descripcion]),
    length(ListaSensores, Longitud),
    format('        - Nº de sensores: ~w\n', [Longitud]),
    imprimir_sensores(ListaSensores).


%
imprimir_ficha_thing(Id) :-
    thing(Id, _, _, ListaSensores),
    imprimir_estado(ListaSensores).

% Función de transición para el simulador
transicion_estado([]).
transicion_estado([IdSensor|Resto]) :-
    transicion_estado_sensor(IdSensor),
    transicion_estado(Resto).

simulador_thing(Id) :-
    thing(Id, _, _, ListaSensores),
    transicion_estado(ListaSensores).