:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

request_meteo(JSON) :-
    setup_call_cleanup(
       http_open('https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current_weather=true', In, []),
       json_read_dict(In, JSON),
       close(In)
   ).

:- dynamic(temperature/1).
:-dynamic(wind_speed/1).
:-dynamic(wind_direction/1).
temperature(0).
wind_speed(0).
wind_direction(0).

current(JSON, CW) :- CW = JSON.get(current_weather). 

update_temp(CW) :- Temp=CW.get(temperature), retractall(temperature(_)), assert(temperature(Temp)).
update_windspeed(CW) :- Windspeed=CW.get(windspeed), retractall(wind_speed(_)), assert(wind_speed(Windspeed)).
update_winddirection(CW) :- Winddir=CW.get(winddirection), retractall(wind_direction(_)), assert(wind_direction(Winddir)).

add_facts_to_database(_):-
   request_meteo(JSON),
   current(JSON, CW),
   update_temp(CW),
   update_windspeed(CW),
   update_winddirection(CW). 


