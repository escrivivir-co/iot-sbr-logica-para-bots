:- module(thingSIM, [
    thing/2,
    thing_type/2,
    thing_location/2,
    thing_status/2,
    update_thing_status/3,
    export_thing_status/2,
    validate_thing_status/2
]).

:- use_module(library(http/json)).

% Define things
thing(thermometer1, thermometer).
thing(lightbulb1, lightbulb).
thing(door1, door).

% Define thing types
thing_type(thermometer, sensor).
thing_type(lightbulb, actuator).
thing_type(door, actuator).

% Define thing locations
thing_location(thermometer1, living_room).
thing_location(lightbulb1, bedroom).
thing_location(door1, front_door).

% Define thing statuses
:- dynamic thing_status/2.

thing_status(thermometer1, 20). % temperature in Celsius
thing_status(lightbulb1, off).
thing_status(door1, closed).

% Validate thing status
validate_thing_status(Thing, Status) :-
    thing(Thing, ThingType),
    (   ThingType = thermometer
    ->  number(Status)
    ;   ThingType = lightbulb
    ->  member(Status, [on, off])
    ;   ThingType = door
    ->  member(Status, [open, closed])
    ;   throw(error(invalid_thing_type(ThingType), _))
    ).

% Update thing status
update_thing_status(Thing, NewStatus, Result) :-
    (   thing(Thing, _)
    ->  (   validate_thing_status(Thing, NewStatus)
        ->  (   retract(thing_status(Thing, _))
            ->  assert(thing_status(Thing, NewStatus)),
                Result = success
            ;   throw(error(status_update_failed(Thing), _))
            )
        ;   throw(error(invalid_status(Thing, NewStatus), _))
        )
    ;   throw(error(thing_not_found(Thing), _))
    ).

% Convert thing status to JSON
thing_status_to_json(Thing, JSON) :-
    (   thing(Thing, ThingType)
    ->  (   thing_status(Thing, Status)
        ->  (   thing_type(ThingType, Type),
                thing_location(Thing, Location)
            ->  JSON = json([
                    thing=Thing,
                    type=Type,
                    location=Location,
                    status=Status
                ])
            ;   throw(error(missing_thing_info(Thing), _))
            )
        ;   throw(error(status_not_found(Thing), _))
        )
    ;   throw(error(thing_not_found(Thing), _))
    ).

% Export thing status as JSON
export_thing_status(Thing, JSONString) :-
    (   thing_status_to_json(Thing, JSON)
    ->  (   catch(atom_json_term(JSONString, JSON, [as(string)]),
                  Error,
                  (print_message(error, Error), fail))
        ->  true
        ;   throw(error(json_conversion_failed(Thing), _))
        )
    ;   throw(error(json_creation_failed(Thing), _))
    ).
