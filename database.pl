:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

request_weather(JSON) :-
    setup_call_cleanup(
       http_open('http://api.weatherapi.com/v1/current.json?key=d7343302f550499f88f11158230704&q=Vancouver', In, []),
       json_read_dict(In, JSON),
       close(In)
   ).

:- dynamic(temperature/1).
:-dynamic(wind_speed/1).
:- dynamic(wind_gust/1).
:-dynamic(wind_direction/1).
:-dynamic(pressure/1).
:-dynamic(precipitation/1).
:-dynamic(humidity/1).
:-dynamic(cloud/1).
:-dynamic(feelslike/1).
:-dynamic(visibility/1).
:-dynamic(uv/1).

temperature(0).
wind_speed(0).
wind_gust(0).
wind_direction("E").
pressure(0). % mb
precipitation(0). % mm
humidity(0).
cloud(0).
feelslike(0).
visibility(0). % km
uv(0).

get_current(JSON, CW) :- CW = JSON.get(current). 

update_database(Name, Value) :-
   Fact1 =.. [Name, _],
   Fact2 =.. [Name, Value],
   retractall(Fact1),
   assert(Fact2).

update_weather(CW) :-
   update_database(temperature, CW.get(temp_c)),
   update_database(wind_speed, CW.get(wind_kph)),
   update_database(wind_gust, CW.get(gust_kph)),
   update_database(wind_direction, CW.get(wind_dir)),
   update_database(pressure, CW.get(pressure_mb)),
   update_database(precipitation, CW.get(precip_mm)),
   update_database(humidity, CW.get(humidity)),
   update_database(cloud, CW.get(cloud)),
   update_database(feelslike, CW.get(feelslike_c)),
   update_database(visibility, CW.get(vis_km)),
   update_database(uv, CW.get(uv)).

add_facts_to_database(_):-
   request_weather(JSON),
   get_current(JSON, CW),
   update_weather(CW).


