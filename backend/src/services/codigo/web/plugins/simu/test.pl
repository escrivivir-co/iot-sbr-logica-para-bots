:- begin_tests(iot_logic_engine).
:- use_module(thing).
:- use_module(simulador).

% Tests for thing module
test(thing_exists) :-
    thing(thermometer1, thermometer),
    thing(lightbulb1, lightbulb),
    thing(door1, door).

test(thing_type) :-
    thing_type(thermometer, sensor),
    thing_type(lightbulb, actuator),
    thing_type(door, actuator).

test(thing_location) :-
    thing_location(thermometer1, living_room),
    thing_location(lightbulb1, bedroom),
    thing_location(door1, front_door).

test(validate_thing_status) :-
    validate_thing_status(thermometer1, 20),
    validate_thing_status(lightbulb1, on),
    validate_thing_status(door1, closed),
    \+ validate_thing_status(thermometer1, on),
    \+ validate_thing_status(lightbulb1, 20),
    \+ validate_thing_status(door1, temperature).

test(update_thing_status) :-
    update_thing_status(thermometer1, 25, Result),
    Result == success,
    thing_status(thermometer1, 25).

test(export_thing_status) :-
    export_thing_status(thermometer1, JSON),
    atom_json_term(JSON, Term, []),
    memberchk(thing=thermometer1, Term),
    memberchk(type=sensor, Term),
    memberchk(location=living_room, Term).

% Tests for simulador module
test(simulate_temperature_change) :-
    simulate_temperature_change(thermometer1, 30),
    thing_status(thermometer1, 30).

test(simulate_light_toggle) :-
    simulate_light_toggle(lightbulb1, on),
    thing_status(lightbulb1, on),
    simulate_light_toggle(lightbulb1, off),
    thing_status(lightbulb1, off).

test(simulate_door_action) :-
    simulate_door_action(door1, open),
    thing_status(door1, open),
    simulate_door_action(door1, closed),
    thing_status(door1, closed).

% Error handling tests
test(invalid_thing, [throws(error(thing_not_found(_),_))]) :-
    update_thing_status(invalid_thing, 20, _).

test(invalid_status, [throws(error(invalid_status(_,_),_))]) :-
    update_thing_status(thermometer1, invalid_status, _).

test(invalid_thermometer, [throws(error(invalid_thermometer(_),_))]) :-
    simulate_temperature_change(lightbulb1, 25).

test(invalid_light_status, [throws(error(invalid_light_status(_),_))]) :-
    simulate_light_toggle(lightbulb1, invalid_status).

test(invalid_door_status, [throws(error(invalid_door_status(_),_))]) :-
    simulate_door_action(door1, invalid_status).

:- end_tests(iot_logic_engine).
