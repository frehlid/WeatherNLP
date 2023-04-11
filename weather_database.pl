% database handles the API calls, and the database of weather facts
:- [database].


% ---------------- Usage ----------------
% To get started, ask a question with:
% ?- q(_). -> 
% The user interface will prompt you to enter a location, and then your questions about the weather!


% ---------------- Grammar Definitions ----------------
% - Please note that the grammar structure is based on 'geography_string.pl' from lecture

% aphrase is true if L0, L1 is a noun_phrase or a modifying phrase
% C0, C1 are the constraints parsed from the phrase
aphrase(L0, L1, E, C0,C1) :- noun_phrase(L0, L1, E,C0,C1).
aphrase(L0, L1, E,C0,C1) :- mp(L0, L1, E,C0,C1).

% noun_phrase is true if L0, L1 is a determiner, a list of adjectives, a noun, and an optional modifying phrase
noun_phrase(L0,L4,Ind,C0,C4) :-
    det(L0,L1,Ind,C3,C4),
    adjectives(L1,L2,Ind,C2,C3),
    noun(L2,L3,Ind,C1,C2),
    omp(L3,L4,Ind,C0,C1).

% a noun_phrase can also have a pronoun instead of a noun
noun_phrase(L0,L4,Ind,C0,C4) :-
    det(L0,L1,Ind,C3,C4),
    adjectives(L1,L2,Ind,C2,C3),
    pronoun(L2,L3,Ind,C1,C2),
    omp(L3,L4,Ind,C0,C1).

% det is true if the first word in L is a determiner
det(["the" | L],L,_,C,C).
det(["a" | L],L,_,C,C).
det(["an" | L],L,_,C,C).
det(L,L,_,C,C). % det is optional

% adjectives are a list of adjectives or nothing
adjectives(L0,L2,Ind,C0,C2) :-
	adj(L0,L1,Ind,C0,C1),
	adjectives(L1,L2,Ind,C1,C2).
adjectives(L,L,_,C,C).

% A modifying phrase is a relation followed by a noun phrase
mp(L0,L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).

% An optional modifying phrase is either a modifying phrase or nothing
omp(L0,L1,E,C0,C1) :-
    mp(L0,L1,E,C0,C1).
omp(L,L,_,C,C).


% ---------------- Dictionary ----------------
% adjectives impose a constraint on the weather fact
adj(["cold" | L],L,Ind, [cold(Ind) | C],C).
adj(["warm" | L],L,Ind, [warm(Ind) | C],C).
adj(["hot" | L],L,Ind, [hot(Ind) | C],C).
adj(["windy" | L],L,Ind, [windy(Ind) | C],C).
adj(["humid" | L],L,Ind, [humid(Ind) | C],C).
adj(["cloudy" | L],L,Ind, [cloudy(Ind) | C],C).
adj(["rainy" | L],L,Ind, [rainy(Ind) | C],C).
adj(["sunny" | L],L,Ind, [sunny(Ind) | C],C).

% adj_to_noun relates adjectives to nouns -- what noun does the adjective modify, and what is the corresponding fact in the database
% adj_to_noun is true if there is a noun related to the corresponding adjective
adj_to_noun("cold", T, "temperature") :- temperature(T).
adj_to_noun("warm", T, "temperature") :- temperature(T).
adj_to_noun("hot", T, "temperature") :- temperature(T).
adj_to_noun("windy", S,  "wind speed") :- wind_speed(S).
adj_to_noun("humid", H, "humidity") :- humidity(H).
adj_to_noun("cloudy", C, "cloud cover") :- cloud(C).
adj_to_noun("snowy", S, "chance of snow") :- chance_of_snow(S).
adj_to_noun("rainy", R, "chance of rain") :- chance_of_rain(R).
adj_to_noun("sunny", C, "cloud cover") :- cloud(C).

% nouns refer to facts in the database
% noun is true if the first word in L is a noun, and the corresponding fact is in the database
% nouns impose a constraint on the weather fact
noun(["temperature" | L],L,Ind, [temperature(Ind) | C],C).
noun(["wind", "speed" | L],L,Ind, [wind_speed(Ind) | C],C).
noun(["wind speed" | L], L, Ind, [wind_speed(Ind) | C],C). % for adj to noun
noun(["wind", "gust" | L],L,Ind, [wind_gust(Ind) | C],C).
noun(["wind", "direction" | L],L,Ind, [wind_direction(Ind) | C],C).
noun(["humidity" | L],L,Ind, [humidity(Ind) | C],C).
noun(["pressure" | L],L,Ind, [pressure(Ind) | C],C).
noun(["wind", "chill" | L],L,Ind, [feelslike(Ind) | C],C).
noun(["visibility" | L],L,Ind, [visibility(Ind) | C],C).
noun(["uv" | L],L,Ind, [uv(Ind) | C],C).
noun(["ultraviolet" | L],L,Ind, [uv(Ind) | C],C).
noun(["cloud" | L],L,Ind, [cloud(Ind) | C],C).
noun(["cloud cover" | L], L, Ind, [cloud(Ind) | C],C).
noun(["precipitation" | L],L,Ind, [precipitation(Ind) | C],C).
noun(["chance of rain" | L],L,Ind, [chance_of_rain(Ind) | C],C).
noun(["chance of snow" | L],L,Ind, [chance_of_rain(Ind) | C],C).
noun(["rain" | L],L,Ind, [rain(Ind) | C],C).
noun(["snow" | L],L,Ind, [snow(Ind) | C],C).

% nouns can also be personal items
% noun is true if the first word in L is a noun, and the corresponding fact is in the database
% personal items impose a constraint on the weather fact
noun(["umbrella" | L], L, Ind, [umbrella(Ind) | C], C).
noun(["coat" | L], L, Ind, [coat(Ind) | C], C).
noun(["raincoat" | L], L, Ind, [raincoat(Ind) | C], C).
noun(["sunglasses" | L], L, Ind, [sunny(Ind) | C], C).
noun(["sunscreen" | L], L, Ind, [sunscreen(Ind) | C], C).
noun(["hat" | L], L, Ind, [hat(Ind) | C], C).
noun(["gloves" | L], L, Ind, [gloves(Ind) | C], C).
noun(["scarf" | L], L, Ind, [scarf(Ind) | C], C).
noun(["boots" | L], L, Ind, [boots(Ind) | C], C).
noun(["jacket" | L], L, Ind, [jacket(Ind) | C], C).


% verb_prhase is true is true if the phrase is "is it". Used for How questions.
verb_phrase(["is", "it" | L], L, _, C, C).

% it can also be a pronoun in some sentences
% pronoun is true if the first word in L is a pronoun
pronoun(["it" |L], L, _, C, C).

% reln is true if the first word in L is a relation, and the corresponding fact is in the database
% reln imposes a constraint on the weather fact
reln(["in" | L],L,Ind,Ind,C,C).
reln(["the", "temperature", "in" | L],L,Sub,_, [temperature(Sub)|C],C).
reln(["the", "wind", "speed", "in" | L],L,Sub,_, [wind_speed(Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,_, [humidity(Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,_, [humidity(Sub)|C],C).
reln(["the", "pressure", "in" | L],L,Sub,_, [pressure(Sub)|C],C).
reln(["the", "wind", "chill", "in" | L],L,Sub,_, [feels_like(Sub)|C],C).

% ---------------- Question structure ----------------
% question is true if the first word in L is a question word, and the corresponding fact is in the database

% if a question starts with "is", it is followed by a phrase followed by an adjective and an optinal modifying phrase
question(["is" | L0],L3,Ind,C0,C3) :-
    aphrase(L0,L1, Ind, C0,C1),
    adj(L1,L2,Ind,C1,C2),
    omp(L2, L3, Ind, C2, C3).

% if a question starts with "what is", it is followed by a phrase.
question(["what", "is" | L0],L1,Ind,C0,C1) :-
    aphrase(L0,L1,Ind,C0,C1).

% questions related to personal items are followed by a noun_phrase
question(["will", "i", "need" | L0],L1,Ind,C0,C1) :-
	noun_phrase(L0, L1, Ind, C0, C1).
question(["do", "i", "need" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).
question(["should", "i", "bring" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).
question(["should", "i", "wear" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).

% questions that start with "will it" are followed by a noun
question(["will", "it" | L0],L1,Ind,C0,C1) :-
	noun(L0,L1,Ind,C0,C1).

% questions that start with "how" are followed by an adjective and a verb phrase
question(["how" | L0],L2,Ind,C0,C2) :-
    adj(L0,L1,Ind,C0,C1),
    verb_phrase(L1, L2, Ind, C1, C2).
% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    get_constraints_from_question(Q,Ind,C),
    prove_all(Q,C, A, Ind), !.

% get_constraints_from_question(Q,A,C) is true if C is the constraints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    question(Q,End,A,C,[]),
    member(End,[[],["?"],["."]]). % check that the question ends with a period


% ---------------- Answer structure ----------------
% prove_all answers the question based on the parsed constraints

% if the question starts with "is", it is a yes or no question
% We will answer Yes / No, and come up with an explanation based on the constraints
% "NO" case
prove_all(["is"| _],[Last],_, _) :-
    \+ call(Last), % constraint is not true
    adj([Adj | _],_,_, [Last | _],_), 
    atom_concat("No, it is not ", Adj, PrintAns1), 
    atom_concat(PrintAns1, "\n", PrintAns),
    write(PrintAns),
    flush_output(current_output),
    adj_to_noun(Adj, Val, Noun),
    noun([Noun | _], _, _, [Constraint | _], _),
    explanation(Noun, Constraint, Val, PrintExp),
    write(PrintExp), flush_output(current_output).

% "YES" case
prove_all(["is"| _],[Last],_, _) :-
    call(Last), % constraint is true
    adj([Adj | _],_,_, [Last | _],_),
    atom_concat("Yes, it is ", Adj, PrintAns1), 
    atom_concat(PrintAns1, "\n", PrintAns),
    write(PrintAns), flush_output(current_output),
    adj_to_noun(Adj, Val, Noun),
    noun([Noun | _], _, _, [Constraint | _], _),
    explanation(Noun, Constraint, Val, PrintExp), 
    write(PrintExp), flush_output(current_output). 

% if the question starts with "will it", is a yes or no question
% no explanation is given, as the question is boolean
% "NO" case
prove_all(["will", "it" | _],[Last],_, _) :-
	\+ call(Last), % constraint is not true
	noun([Noun | _],_,_, [Last | _],_),
	atom_concat("No, it will not ", Noun, PrintAns1), 
	atom_concat(PrintAns1, "\n", PrintAns),
	write(PrintAns), flush_output(current_output).

% "YES" case
prove_all(["will", "it" | _],[Last],_, _) :-
	call(Last), % constraint is true
	noun([Noun | _],_,_, [Last | _],_),
	atom_concat("Yes, it will ", Noun, PrintAns1), 
	atom_concat(PrintAns1, "\n", PrintAns),
	write(PrintAns), flush_output(current_output).


% questions that are related to personal items are handled by accessory_questions
prove_all(["should", "i", "bring" | _], [Last],_, _) :-
    accessory_questions(Last, "should", "bring").

prove_all(["should", "i", "wear" | _], [Last],_, _) :-
    accessory_questions(Last, "should", "wear").

prove_all(["do", "i", "need" | _], [Last],_, _) :-
    accessory_questions(Last, "do", "need").

% if the question starts with "what" it is asking for a value
prove_all(["what"| _],[Last],_, Ind) :-
    call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    explanation(Noun, Last, Ind, PrintAns),
    write(PrintAns), flush_output(current_output).

% if the question starts with "how" it might be asking for a value
prove_all(["how"| _],[Last],_, _) :-
    adj([Adj | _],_,_, [Last | _],_),
    adj_to_noun(Adj, Val, Noun),
    noun([Noun | _], _, _, [Constraint | _], _),
    explanation(Noun, Constraint, Val, PrintAns),
    write(PrintAns), flush_output(current_output).

prove_all(_,[],_, _).
prove_all(_,[H|T],A, Ind) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(_, T, A, Ind),
    A is Ind.

% accessory_questions answers questions related to personal items
% we also generate an explanation for the answer based on the item
% "YES" case
accessory_questions(Last,  DoOrShould, BringOrWear) :-
    call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    format(atom(PrintAns), "Yes, you ~s ~s a(n) ~s. ", [DoOrShould, BringOrWear, Noun]),
    write(PrintAns),
    flush_output(current_output),
    get_explanation(Noun, _, Exp),
    write(Exp), flush_output(current_output).
% "NO" case
accessory_questions(Last, DoOrShould, BringOrWear) :-
    \+ call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    format(atom(PrintAns), "No, you ~s not ~s a(n) ~s. ", [DoOrShould, BringOrWear, Noun]),
    write(PrintAns),
    flush_output(current_output),
    get_explanation(Noun, _, Exp),
    write(Exp), flush_output(current_output).


% ---------------- Main Prompting Structure ----------------
% q(_) prompts the user for a city and then ask questions
q(_) :-
    write("What city would you like to ask about? "), flush_output(current_output),
    read_line_to_string(user_input, CitySt),
    (add_facts_to_database(CitySt)
    ->  atom_concat(CitySt, "? ", CityQ),
        atom_concat("Great! What weather facts would you like to know about ", CityQ, PrintPrompt),
        write(PrintPrompt), flush_output(current_output),
        ask_questions(_)
    ;   format("We could not find weather data for ~w. Please try again.", [CitySt])).
q(_) :-
   write("Ask another question with q(_). \n").

% ask_questions prompts the user for questions and answers them
ask_questions(_) :-
    read_line_to_string(user_input, StIn),
    downcase_atom(StIn, St),
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    (catch(ask(Ln, _), _, fail) ->
        true ; (write("No answers"), true)),
    write("\nDo you have more questions? (y/n) \n"), flush_output(current_output),
    read_line_to_string(user_input, YesOrNo),
    (YesOrNo == "y" ; YesOrNo == "Y"),
    write("Ask away!\n"),
    ask_questions(_).

% ---------------- Predicates about weather ----------------
% cold(C) is true if the temperature is T and it is below 10 degrees.
cold(T) :- temperature(T), T =< 15.0.

% warm(C) is true if the temperature is T and it is above 10 degrees
warm(T) :- temperature(T), T > 15.0.

% hot(C) is true if the temperature is T and it is above 30 degrees
hot(T) :- temperature(T), T > 30.0.

% windy(S) is true if the wind speed is S and it is above 10 kph
windy(S) :- wind_speed(S), S > 10.0.

% rainy(P) is true if the chance of rain is P and it is above 50%
rainy(P) :- chance_of_rain(P), P > 50.

% humid(H) is true if the humidity is H and it is above 60%
humid(H) :- humidity(H), H > 60.

% cloudy(C) is true if the cloud cover is C and it is above 60%
cloudy(C) :- cloud(C), C > 60.

% sunny(C) is true if the cloud cover is C and it is below 60%
sunny(C) :- cloud(C), C < 60.

coat(C) :- cold(C).

umbrella(C) :- rainy(C).

hat(C) :- sunny(C).

raincoat(C) :- rainy(C).

sunscreen(C) :- uv(C), C > 5.

gloves(C) :- cold(C).

scarf(C) :- cold(C).

boots(C) :- precipitation(C), C > 0.

jacket(C) :- cold(C).

snow(C) :- will_it_snow(C), C > 0.

rain(C) :- will_it_rain(C), C > 0.




