:- module(kitThing, [inicializar_things/0, imprimir_thing/1, imprimir_ficha_thing/1, simulador_thing/1, imprimir_kit/1, imprimir_kit_json/1]).
:- use_module('../../sdk.pl', [thing/4, imprimir_kit/0]).
:- use_module('./sensor.pl', [sensor/7, transicion_estado_sensor/1, sensor_valor_actual/2]).
:- use_module('./reglas.pl', [inicializar_reglas/0, imprimir_reglas/2, buscar_reglas/3]).
:- use_module(library(http/json)).

% ******************** SDK/MODULOS/THING *****************************
% * Funcionalidad: a) set/get b) transicion por consigna *
% ******************** SDK/MODULOS/THING *****************************

inicializar_things :-
    format('[Modulo: Things] ~n', []).

thing_lista_sensores(Id, ListaSensores) :-
    thing(Id, _, _, ListaSensores).

% Impresión en formato de texto
imprimir_sensores([], "").
imprimir_sensores([IdSensor|Resto], Result) :-
    sensor(IdSensor, Nombre, Descripcion, Unidad, ValorActual, ValorConsigna, ValorIncremento),
    format(atom(Line), '        - ~w, Nombre: ~w, ~w, Valor: (~w) ~w, consigna: ~w (~w)\n',
        [IdSensor, Nombre, Descripcion, Unidad, ValorActual, ValorConsigna, ValorIncremento]),
    imprimir_sensores(Resto, RestLines),
    atom_concat(Line, RestLines, Result).

% Impresión del estado en formato de texto
imprimir_estado([], "").
imprimir_estado([IdSensor|Resto], Result) :-
    sensor(IdSensor, _, _, _, ValorActual, ValorConsigna, ValorIncremento),
    format(atom(Line), '        - ~w, Valor: (~w) ~w, consigna: ~w\n',
        [IdSensor, ValorActual, ValorConsigna, ValorIncremento]),
    buscar_reglas(IdSensor, ValorActual, Reglas),
    imprimir_reglas(Reglas, ReglasString),
    imprimir_estado(Resto, RestLines),
    atom_concat(Line, ReglasString, TempResult),
    atom_concat(TempResult, RestLines, Result).

% Impresión en formato de texto
imprimir_thing(Id, Result) :-
    thing(Id, Nombre, Descripcion, ListaSensores),
    format(atom(Header), '    - Thing: (~w),  ~w\n', [Nombre, Descripcion]),
    length(ListaSensores, Longitud),
    format(atom(SensorHeader), '        - Nº de sensores: ~w\n', [Longitud]),
    imprimir_sensores(ListaSensores, SensoresString),
    atom_concat(Header, SensorHeader, TempResult),
    atom_concat(TempResult, SensoresString, Result).

imprimir_ficha_thing(Id, Result) :-
    thing(Id, _, _, ListaSensores),
    imprimir_estado(ListaSensores, Result).

% Versión en JSON
imprimir_thing_json(Id, JsonResult) :-
    thing(Id, Nombre, Descripcion, ListaSensores),
    maplist(sensor_a_json_objeto, ListaSensores, SensoresJson),
    JsonResult = json([id=Id, nombre=Nombre, descripcion=Descripcion, sensores=SensoresJson]).

sensor_a_json_objeto(IdSensor, json([id=IdSensor, nombre=Nombre, descripcion=Descripcion, unidad=Unidad, valorActual=ValorActual, valorConsigna=ValorConsigna, valorIncremento=ValorIncremento])) :-
    sensor(IdSensor, Nombre, Descripcion, Unidad, ValorActual, ValorConsigna, ValorIncremento).

imprimir_kit(JsonString) :-
    findall(ThingJson, (
        thing(Id, Nombre, Descripcion, Sensores),
        imprimir_thing_json(Id, ThingJson)
    ), ThingsJson),
    atom_json_term(JsonString, ThingsJson, []).

imprimir_kit_json(JsonString) :-
    findall(ThingJson, (
        thing(Id, Nombre, Descripcion, Sensores),
        imprimir_thing_json(Id, ThingJson)
    ), ThingsJson),
    atom_json_term(JsonString, ThingsJson, []).
