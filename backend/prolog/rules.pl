% This file will be dynamically updated with user-defined rules
% Example rule:
apply_rules(Result) :- 
    findall(X, (telemetry(Sensor, Value), process_telemetry(Sensor, Value, X)), Result).

process_telemetry(temperature, Value, high_temperature) :- Value > 30.
process_telemetry(humidity, Value, high_humidity) :- Value > 70.

telemetry_status(Status) :-
    findall(sensor(Sensor, Value), telemetry(Sensor, Value), Status).
