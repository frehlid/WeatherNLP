:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

% ----------------- DATABASE  -----------------
% all of the facts are dynamic, so we can assert/retract them at runtime 
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

% Weather Facts
% Current
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

% forecast 
maxtemp(0).
mintemp(0).
avgtemp(0).
maxwind(0).
will_it_rain(0).
chance_of_rain(0).
will_it_snow(0).
chance_of_snow(0).

% Units for generating explanations
% units is true if the constraint is a fact and the unit is a string
units(pressure(_), "mb").
units(temperature(_), "C").
units(wind_speed(_), "kph").
units(precipitation(_), "mm").
units(humidity(_), "%").
units(cloud(_), "%").
units(feelslike(_), "C").
units(visibility(_), "km").
units(uv(_), "UVI").
units(maxtemp(_), "C").
units(avgtemp(_), "C").
units(mintemp(_), "C").
units(will_it_rain(_), "0/1").
units(will_it_snow(_), "0/1").
units(chance_of_rain(_), "%").
units(chance_of_snow(_), "%").
units(sunny(_), "%").


% Explanation generates a string concatinating the Noun, Value, and Constraint units
explanation(Noun, Constraint, Ind, Exp) :-
    atom_concat("The ", Noun, First),
    atom_concat(First, " is ", Second),
    atom_concat(Second, Ind, PrintAns0),
    atom_concat(PrintAns0, " ", PrintAns1),
    units(Constraint, Unit),
    atom_concat(PrintAns1, Unit, Exp).


% umbrella is explained with chance_of_rain
get_explanation("umbrella", _, Exp) :-
    chance_of_rain(Chance),
    explanation("chance of rain", chance_of_rain(_), Chance, Exp).

% coat is explained with temperature
get_explanation("coat", _, Exp) :-
    temperature(Temp),
    explanation("temperature", temperature(_), Temp, Exp).

% sunglasses is explained with cloud cover
get_explanation("sunglasses", _, Exp) :-
    cloud(Clouds),
    explanation("cloud cover", cloud(_), Clouds, Exp).

% raincoat is explained with chance of rain
get_explanation("raincoat", _, Exp) :-
    chance_of_rain(Chance),
    explanation("chance of rain", chance_of_rain(_), Chance, Exp).

% sunscreen is explained with UV index
get_explanation("sunscreen", _, Exp) :-
	uv(UV),
	explanation("UV index", uv(_), UV, Exp).

% hat is explained with cloud cover
get_explanation("hat", _, Exp) :-
	cloud(Clouds),
	explanation("cloud cover", cloud(_), Clouds, Exp).

get_explanation("gloves", _, Exp) :-
	temperature(Temp),
	explanation("temperature", temperature(_), Temp, Exp).

get_explanation("scarf", _, Exp) :-
	temperature(Temp),
	explanation("temperature", temperature(_), Temp, Exp).

get_explanation("boots", _, Exp) :-
	precipitation(P),
	explanation("precipitation", precipitation(_), P, Exp).

get_explanation("jacket", _, Exp) :-
	temperature(Temp),
	explanation("temperature", temperature(_), Temp, Exp).

% replace spaces with dashes for the API
replace_spaces_with_dash(String, Result) :-
    atomic_list_concat(Words, ' ', String),
    atomic_list_concat(Words, '-', Result).

% reuqest weather to weatherapi.com and parse the JSON as a dict
request_weather(City, JSON) :-
    replace_spaces_with_dash(City, CityDash),
    atom_concat('http://api.weatherapi.com/v1/forecast.json?key=d7343302f550499f88f11158230704&q=', CityDash, URL),
    catch(
        setup_call_cleanup( % Request code taken from swi-prolog docs
            http_open(URL, In, []),
            json_read_dict(In, JSON),
            close(In)
        ),
     error(_,http_error(_,_,_)),
     JSON = false).

% returns the 'current' object from the JSON
get_current(JSON, CW) :- CW = JSON.get(current).

% retracts all facts with Name, and asserts a new fact with Name and Value
update_database(Name, Value) :-
   Fact1 =.. [Name, _],
   Fact2 =.. [Name, Value],
   retractall(Fact1),
   assert(Fact2).

% updates the database with the current weather
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

% returns the 'day' object from the JSON
get_day(JSON, Day) :- Forecast = JSON.get(forecast), [FD | _] = Forecast.get(forecastday), Day = FD.get(day).

% updates the database with the forecast
update_forecast(Day) :-
    update_database(maxtemp, Day.get(maxtemp_c)),
    update_database(mintemp, Day.get(mintemp_c)),
    update_database(avgtemp, Day.get(avgtemp_c)),
    update_database(maxwind, Day.get(maxwind_kph)),
    update_database(will_it_rain, Day.get(daily_will_it_rain)),
    update_database(will_it_snow, Day.get(daily_will_it_snow)),
    update_database(chance_of_rain, Day.get(daily_chance_of_rain)),
    update_database(chance_of_snow, Day.get(daily_chance_of_snow)).

% true if the two strings are equal ignoring case
equal_ignore_case(S1, S2) :-
    downcase_atom(S1, S1Lower),
    downcase_atom(S2, S2Lower),
    S1Lower = S2Lower.

% Query the API for the weather of the city, and update the database with the results
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

