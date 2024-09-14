:- module(app, [inicializarMaquina/1, imprimir_reglas/1, imprimir_reglas_json/1, imprimir_kit/0, imprimir_kit_json/1, sensor_valor_actual_guardar/2]).

:- use_module('sdk/sdk.pl', [activar_sdk/0, thing/4, regla/5, sensor/7, imprimir_reglas/1, imprimir_reglas_json/1, imprimir_kit/0, imprimir_kit_json/1]).
:- use_module('sdk/modulos/dominio/thing.pl', []).
:- use_module('sdk/modulos/dominio/sensor.pl', [sensor_valor_actual_guardar/2, sensor_valor_actual_guardar/3]).
:- use_module('sdk/modulos/simulador/simulador.pl', [parar/0]).
:- use_module(library(random)).

% Constantes para los dispositivos
sensorMotor.
sensorTrabajo.
thingRobot.

sensorDeposito.
thingDeposito.

% *********************************************************
% * Condición para el Sensor Depósito
% *********************************************************
esSensorDeposito(IdSensor) :- IdSensor = sensorDeposito.
quedaCapacidad(NuevoValor) :-
    sensor(sensorDeposito, _, _, _, _, ValorConsigna, _),
    Capacidad is ValorConsigna - NuevoValor,
    Capacidad > 0.

motorTrabajando :-
    sensor(sensorTrabajo, _, _, _, ValorActual, _, _),
    ValorActual > 0.

% Predicado principal
condicionDeposito(IdSensor, NuevoValor) :-
    esSensorDeposito(IdSensor),
    quedaCapacidad(NuevoValor),
    motorTrabajando.

% *********************************************************
% * Acción para el Sensor Depósito
% *********************************************************
capacidadDisponible(IdSensor, Capacidad, ValorActual) :-
    sensor(IdSensor, _, _, _, ValorActual, ValorConsigna, _),
    Capacidad is (ValorConsigna - ValorActual).

optimizarCapacidad(Capacidad, ValorActual, ValorOptimizado) :-
    % Agregamos una cantidad aleatoria de carga de trabajo
    random_between(0, Capacidad, ValorRandom),
    ValorOptimizado is ValorActual + ValorRandom.

verificarCapacidad(ValorOptimizado) :-
    sensor(sensorTrabajo, _, _, _, CargaActual, _, _),
    CargaActual > ValorOptimizado.

% Predicado principal
accionDeposito(_, IdSensor, _) :-
    capacidadDisponible(IdSensor, Capacidad, ValorActual),
    optimizarCapacidad(Capacidad, ValorActual, ValorOptimizado),
    verificarCapacidad(ValorOptimizado),
    CargaAgregada is ValorOptimizado - ValorActual,
    format('[Se produce carga de buffer, valor optimizado: ~w] ~n', [CargaAgregada]),
    sensor_valor_actual_guardar(IdSensor, ValorOptimizado).

% *********************************************************
% * Condición para el Sensor Motor
% *********************************************************
condicionMotor(IdSensor, NuevoValor) :-
    IdSensor = sensorMotor,
    sensor(IdSensor, _, _, _, _, ValorConsigna, _),
    NuevoValor =:= ValorConsigna.

% *********************************************************
% * Acción para el Sensor Motor
% *********************************************************
transicionMotor(IdSensor) :-
    sensor(sensorMotor, _, _, _, _, ValorConsigna, ValorIncremento),
    NuevaConsigna is ValorConsigna * -1,
    NuevoIncremento is ValorIncremento,
    format('[El motor ha concluído un ciclo de trabajo.] ~n', []),
    sensor_valor_actual_guardar(IdSensor, NuevaConsigna, NuevoIncremento).

transicionTrabajo :-
    sensor(sensorDeposito, _, _, _, ValorActual, _, _),
    ValorActual > 0,
    NuevoValor is ValorActual - 1, % Simular procesar una carga de trabajo
    format('[Se retira una unidad de carga de trabajo, quedan  ~w.] ~n', [NuevoValor]),
    sensor_valor_actual_guardar(sensorDeposito, NuevoValor).

transicionDeposito(CargaDeTrabajoRestante) :-
    sensor(sensorTrabajo, _, _, _, CargaActual, _, _),
    CargaDeTrabajoRestante is CargaActual - 1,
    sensor_valor_actual_guardar(sensorTrabajo, CargaDeTrabajoRestante).

% Predicado principal
accionMotor(_, IdSensor, _) :-
    % El motor procesará una unidad de "carga" por cada ciclo de "trabajo" completado.
    transicionMotor(IdSensor),
    % El contenedor mostrará el buffer de "carga" esperando ser procesado.
    transicionTrabajo,
    % El contador total de carga indicará la cantidad total pendiente de trabajo.
    transicionDeposito(CargaDeTrabajoRestante),

    % Si no queda carga de trabajo, detener
    CargaDeTrabajoRestante =:= 1,
    format('[No queda carga de trabajo, el motor se parará.] ~n', []),
    parar.

% *********************************************************
% * Carga de dominio y reglas
% *********************************************************
inicializar_things :-
    assert(sensor(sensorMotor, 'Ciclo', 'Procesa una carga de trabajo por ciclo', 'nº de paso', -1, 1, 1)),
    assert(sensor(sensorTrabajo, 'Trabajo', 'Contador de carga de trabajo pendiente', 'nº paquetes', 20, 20, 0)),
    assert(thing(thingRobot, 'Robot', 'Modeliza un robot que procesa "cargas" de trabajo', [sensorMotor, sensorTrabajo])),

    assert(sensor(sensorDeposito, 'Carga', 'Contador de cargas', 'nº paquetes', 0, 10, 0)),
    assert(thing(thingDeposito, 'Contenedor', 'Almacén inteligente', [sensorDeposito])).

inicializar_reglas :-
    assert(regla(1, condicionMotor, accionMotor, 'Revoluciones del Motor', true)),
    assert(regla(2, condicionDeposito, accionDeposito, 'Optimizador de Carga', true)).

% *********************************************************
% * Arranque de aplicación
% *********************************************************
% :- format('[Modulo: Aplicación] ~n', []),
    % inicializar_things,
    % inicializar_reglas,
    % activar_sdk.

inicializarMaquina(Result) :-
	inicializar_things,
    inicializar_reglas,
	imprimir_reglas(Result). %,
    % activar_sdk.