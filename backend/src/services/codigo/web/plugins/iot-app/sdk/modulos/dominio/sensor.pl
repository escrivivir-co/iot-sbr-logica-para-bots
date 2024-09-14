:- module(sensor, [sensor/7, transicion_estado_sensor/1,
    sensor_valor_actual/2, sensor_valor_actual_guardar/2,
    sensor_valor_actual_guardar/3]).
:- use_module('../../sdk.pl', [sensor/7]).

% ******************** SDK/MODULOS/SENSOR *****************************
% * Funcionalidad: a) set/get b) transicion por consigna *
% ******************** SDK/MODULOS/SENSOR *****************************

% Predicados para obtener y establecer el valor del sensor
sensor_valor_actual(Id, Valor) :-
    sensor(Id, _, _, _, Valor, _, _).

sensor_valor_actual_guardar(Id, NuevoValor) :-
	format('[Modulo: Actualizar: (~w)] ~n', [Id]),
    sensor(Id, Nombre, Descripcion, Unidad, _, ValorConsigna, ValorIncremento),
	format('[Modulo: Encontrado: (~w, ~w)] ~n', [Id, Nombre]),
    retract(sensor(Id, _, _, _, _, _, _)),
    assert(sensor(Id, Nombre, Descripcion, Unidad, NuevoValor, ValorConsigna, ValorIncremento)).

sensor_valor_actual_guardar(Id, Consigna, Incremento) :-
    sensor(Id, Nombre, Descripcion, Unidad, Valor, _, _),
    retract(sensor(Id, _, _, _, _, _, _)),
    assert(sensor(Id, Nombre, Descripcion, Unidad, Valor, Consigna, Incremento)).

% Predicado para actualizar la simulación del sensor.
% En este caso simulamos un mecanismo de consigna:
% dado un valor deseado, el valor actual se incrementa
% para aproximarse a la consigna
transicion_estado_sensor(Id) :-
    sensor(Id, _, _, _, ValorActual, ValorConsigna, ValorIncremento),
    (
        (ValorActual < ValorConsigna, NuevoValor is ValorActual + ValorIncremento, sensor_valor_actual_guardar(Id, NuevoValor));
        (ValorActual > ValorConsigna, NuevoValor is ValorActual - ValorIncremento, sensor_valor_actual_guardar(Id, NuevoValor));
        (ValorActual =:= ValorConsigna)
    ).
