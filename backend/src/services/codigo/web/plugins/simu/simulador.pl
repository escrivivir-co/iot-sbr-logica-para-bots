:- module(simSIM, [
    simulate_temperature_change/2,
    simulate_light_toggle/2,
    simulate_door_action/2
]).

:- use_module(thing).

% Simulate temperature change
simulate_temperature_change(Thermometer, NewTemperature) :-
    catch(
        (   thing(Thermometer, thermometer)
        ->  (   update_thing_status(Thermometer, NewTemperature, Result)
            ->  format('Temperature change simulation result: ~w~n', [Result])
            ;   throw(error(update_failed(Thermometer, NewTemperature), _))
            )
        ;   throw(error(invalid_thermometer(Thermometer), _))
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).

% Simulate light toggle
simulate_light_toggle(Lightbulb, NewStatus) :-
    catch(
        (   thing(Lightbulb, lightbulb)
        ->  (   member(NewStatus, [on, off])
            ->  (   update_thing_status(Lightbulb, NewStatus, Result)
                ->  format('Light toggle simulation result: ~w~n', [Result])
                ;   throw(error(update_failed(Lightbulb, NewStatus), _))
                )
            ;   throw(error(invalid_light_status(NewStatus), _))
            )
        ;   throw(error(invalid_lightbulb(Lightbulb), _))
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).

% Simulate door action
simulate_door_action(Door, NewStatus) :-
    catch(
        (   thing(Door, door)
        ->  (   member(NewStatus, [open, closed])
            ->  (   update_thing_status(Door, NewStatus, Result)
                ->  format('Door action simulation result: ~w~n', [Result])
                ;   throw(error(update_failed(Door, NewStatus), _))
                )
            ;   throw(error(invalid_door_status(NewStatus), _))
            )
        ;   throw(error(invalid_door(Door), _))
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).
