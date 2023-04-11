:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

% Wikidata endpoint URL
wdqs_url('https://query.wikidata.org/sparql').

% SPARQL query to check if a city exists in Wikidata
query(City, Sparql) :-
    atomic_list_concat(['SELECT ?city WHERE { ?city wdt:P31/wdt:P279* wd:Q515. ?city rdfs:label "', City, '"@en. }'], Sparql).

replace_spaces_with_dash(String, Result) :-
    atomic_list_concat(Words, ' ', String),
    atomic_list_concat(Words, '-', Result).

%request_weather(City, JSON) :-
%    replace_spaces_with_dash(City, CityDash),
%    atom_concat('http://api.weatherapi.com/v1/current.json?key=d7343302f550499f88f11158230704&q=', CityDash, URL),
%    setup_call_cleanup(
%       http_open(URL, In, []),
%       json_read_dict(In, JSON),
%       close(In)
%   ).
request_weather(City, JSON) :-
    replace_spaces_with_dash(City, CityDash),
    atom_concat('http://api.weatherapi.com/v1/forecast.json?key=d7343302f550499f88f11158230704&q=', CityDash, URL),
    catch(
        setup_call_cleanup(
            http_open(URL, In, []),
            json_read_dict(In, JSON),
            close(In)
        ),
     error(_,http_error(_,_,_)),
     JSON = false).




:- dynamic(location/1).
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

:-dynamic(maxtemp/1).
:-dynamic(mintemp/1).
:-dynamic(avgtemp/1).
:-dynamic(maxwind/1).
:-dynamic(will_it_rain/1).
:-dynamic(chance_of_rain/1).
:-dynamic(will_it_snow/1).
:-dynamic(chance_of_snow/1).


units(pressure(_), "mb").
units(temperature(_), "C").
units(wind_speed(_), "kph").
units(precipitation(_), "mm").
units(humidity(_), "%").
units(cloud(_), "%").
units(feelslike(_), "C").
units(visibility(_), "km").
units(uv(_), "UV Index").
units(maxtemp(_), "C").
units(avgtemp(_), "C").
units(mintemp(_), "C").
units(will_it_rail(_), "0/1").
units(will_it_snow(_), "0/1").
units(chance_of_rain(_), "%").
units(chance_of_snow(_), "%").
units(sunny(_), "%").


explanation(Noun, Constraint, Ind, Exp) :-
    atom_concat("The ", Noun, First),
    atom_concat(First, " is ", Second),
    atom_concat(Second, Ind, PrintAns0),
    atom_concat(PrintAns0, " ", PrintAns1),
    units(Constraint, Unit),
    atom_concat(PrintAns1, Unit, Exp).



get_explanation("umbrella", _, Exp) :-
    chance_of_rain(Chance),
    explanation("chance of rain", chance_of_rain(_), Chance, Exp).

get_explanation("coat", _, Exp) :-
    temperature(Temp),
    explanation("temperature", temperature(_), Temp, Exp).

get_explanation("sunglasses", _, Exp) :-
    cloud(Clouds),
    explanation("cloud cover", cloud(_), Clouds, Exp).

get_explanation("raincoat", _, Exp) :-
    chance_of_rain(Chance),
    explanation("chance of rain", chance_of_rain(_), Chance, Exp).


location("").
temperature(0).
wind_speed(0).
wind_gust(0).

pressure(0). % mb
precipitation(0). % mm
humidity(0).
cloud(0).
feelslike(0).
visibility(0). % km
uv(0).

maxtemp(0).
mintemp(0).
avgtemp(0).
maxwind(0).
will_it_rain(0).
chance_of_rain(0).
will_it_snow(0).
chance_of_snow(0).

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

get_day(JSON, Day) :- Forecast = JSON.get(forecast), [FD | _] = Forecast.get(forecastday), Day = FD.get(day).

update_forecast(Day) :-
    update_database(maxtemp, Day.get(maxtemp_c)),
    update_database(mintemp, Day.get(mintemp_c)),
    update_database(avgtemp, Day.get(avgtemp_c)),
    update_database(maxwind, Day.get(maxwind_kph)),
    update_database(will_it_rain, Day.get(daily_will_it_rain)),
    update_database(will_it_snow, Day.get(daily_will_it_snow)),
    update_database(chance_of_rain, Day.get(daily_chance_of_rain)),
    update_database(chance_of_snow, Day.get(daily_chance_of_snow)).


%add_facts_to_database(City):-
%   request_weather(City, JSON),
%   get_current(JSON, CW),
%   update_weather(CW).

equal_ignore_case(S1, S2) :-
    downcase_atom(S1, S1Lower),
    downcase_atom(S2, S2Lower),
    S1Lower = S2Lower.


add_facts_to_database(City) :-
    catch(request_weather(City, JSON), _, fail),
    Location = JSON.get(location),
    CityR = Location.get(name),
    (equal_ignore_case(CityR, City) ->
        update_database(location, City),
        get_current(JSON, CW),
        update_weather(CW),
        get_day(JSON, Day),
        update_forecast(Day)
    ;
        fail
    ).

