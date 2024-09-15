:- module(app, [main/0, mainObj/1, simulate_temperature/2, thing_status/2]).
:- use_module(library(http/json), [atom_json_term/3]).
:- use_module('./thing.pl').
:- use_module('./simulador.pl').

% Main program
main :-
    format('Starting IoT Logic Engine simulation...~n'),
    
    % Simulate temperature change
    (   catch(simulate_temperature_change(thermometer1, 22),
              Error,
              (print_message(error, Error), fail))
    ->  format('Temperature change simulation successful.~n')
    ;   format('Temperature change simulation failed.~n'), fail
    ),
    
    % Simulate light toggle
    (   catch(simulate_light_toggle(lightbulb1, on),
              Error,
              (print_message(error, Error), fail))
    ->  format('Light toggle simulation successful.~n')
    ;   format('Light toggle simulation failed.~n'), fail
    ),
    
    % Simulate door action
    (   catch(simulate_door_action(door1, open),
              Error,
              (print_message(error, Error), fail))
    ->  format('Door action simulation successful.~n')
    ;   format('Door action simulation failed.~n'), fail
    ),
    
    % Export thing statuses as JSON
    format('Starting to export thing statuses...~n'),
    (   catch(export_thing_status(thermometer1, ThermometerJSON),
              Error,
              (print_message(error, Error), fail))
    ->  format('Thermometer status exported successfully.~n')
    ;   format('Failed to export thermometer status.~n'), fail
    ),
    (   catch(export_thing_status(lightbulb1, LightbulbJSON),
              Error,
              (print_message(error, Error), fail))
    ->  format('Lightbulb status exported successfully.~n')
    ;   format('Failed to export lightbulb status.~n'), fail
    ),
    (   catch(export_thing_status(door1, DoorJSON),
              Error,
              (print_message(error, Error), fail))
    ->  format('Door status exported successfully.~n')
    ;   format('Failed to export door status.~n'), fail
    ),
    format('All thing statuses exported successfully.~n'),
    
    % Print JSON strings
    format('Thermometer status: ~w~n', [ThermometerJSON]),
    format('Lightbulb status: ~w~n', [LightbulbJSON]),
    format('Door status: ~w~n', [DoorJSON]),
    
    % Indicate successful completion
    format('IoT Logic Engine simulation completed successfully.~n').


mainObj(JSONString) :-
    format('Starting IoT Logic Engine simulation...~n'),

    % Accumulate the traces
    accumulate_traces([], TraceLog),

    % Create final JSON term with the trace list
    %TraceLogJSON = json{log: TraceLog},

    % Convert the JSON term to a string that Node.js can handle
    atom_json_term(JSONString, TraceLog, []),

    % Print the final JSON log
    format('~w~n', [JSONString]),

    % Indicate successful completion
    format('IoT Logic Engine simulation completed successfully.~n').

% Helper to accumulate the traces
accumulate_traces(Acc, FinalTraceList) :-
    % Simulate temperature change
    (   catch(simulate_temperature_change(thermometer1, 22),
              Error,
              (print_message(error, Error), fail))
    ->  Trace1 = json{thing: thermometer1, action: "temperature_change", status: success}
    ;   Trace1 = json{thing: thermometer1, action: "temperature_change", status: failed}
    ),

    % Simulate light toggle
    (   catch(simulate_light_toggle(lightbulb1, on),
              Error,
              (print_message(error, Error), fail))
    ->  Trace2 = json{thing: lightbulb1, action: "light_toggle", status: success}
    ;   Trace2 = json{thing: lightbulb1, action: "light_toggle", status: failed}
    ),

    % Simulate door action
    (   catch(simulate_door_action(door1, open),
              Error,
              (print_message(error, Error), fail))
    ->  Trace3 = json{thing: door1, action: "door_action", status: success}
    ;   Trace3 = json{thing: door1, action: "door_action", status: failed}
    ),

    % Export thing statuses as JSON
    (   catch(export_thing_status(thermometer1, ThermometerJSON),
              Error,
              (print_message(error, Error), fail))
    ->  Trace4 = json{thing: thermometer1, action: "status_export", status: success, data: ThermometerJSON}
    ;   Trace4 = json{thing: thermometer1, action: "status_export", status: failed}
    ),

    (   catch(export_thing_status(lightbulb1, LightbulbJSON),
              Error,
              (print_message(error, Error), fail))
    ->  Trace5 = json{thing: lightbulb1, action: "status_export", status: success, data: LightbulbJSON}
    ;   Trace5 = json{thing: lightbulb1, action: "status_export", status: failed}
    ),

    (   catch(export_thing_status(door1, DoorJSON),
              Error,
              (print_message(error, Error), fail))
    ->  Trace6 = json{thing: door1, action: "status_export", status: success, data: DoorJSON}
    ;   Trace6 = json{thing: door1, action: "status_export", status: failed}
    ),

    % Accumulate all traces
    FinalTraceList = [Trace1, Trace2, Trace3, Trace4, Trace5, Trace6 | Acc].

simulate_temperature(Thermometer, NewTemperature) :-
	format('Starting IoT Logic Engine simulation...~n ~w/~w', [Thermometer, NewTemperature]),
	simulate_temperature_change(Thermometer, NewTemperature).
