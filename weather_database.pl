/*
	This file answers simple weather questions from the user.
	Currently, the information is just hard-coded as a database.
*/

% database is loaded from
:- [database].

% ------------ Current usage -------------
% ?- ask(["Is", "Vancouver", "cold", "?"], A, C).
% A = cold(vancouver) ;
% true.
% ?-

% ?- ask(["What", "is", "the", "temperature", "in", "Vancouver", "?"], A, C).
% A = temperature(vancouver, 9) ;
% true.

aphrase(L0, L1, E, C0,C1) :- noun_phrase(L0, L1, E,C0,C1).
aphrase(L0, L1, E,C0,C1) :- mp(L0, L1, E,C0,C1).

noun_phrase(L0,L4,Ind,C0,C4) :-
    det(L0,L1,Ind,C3,C4),
    adjectives(L1,L2,Ind,C2,C3),
    noun(L2,L3,Ind,C1,C2),
    omp(L3,L4,Ind,C0,C1).

noun_phrase(L0,L4,Ind,C0,C4) :-
    det(L0,L1,Ind,C3,C4),
    adjectives(L1,L2,Ind,C2,C3),
    pronoun(L2,L3,Ind,C1,C2),
    omp(L3,L4,Ind,C0,C1).

% ---------------- Question Structure ----------------
% the is a determiner
det(["the" | L],L,_,C,C).
det(["a" | L],L,_,C,C).
det(["an" | L],L,_,C,C).
det(L,L,_,C,C).

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
adj(["cold" | L],L,Ind, [cold(Ind) | C],C).
adj(["warm" | L],L,Ind, [warm(Ind) | C],C).
adj(["windy" | L],L,Ind, [windy(Ind) | C],C).
adj(["humid" | L],L,Ind, [humid(Ind) | C],C).
adj(["cloudy" | L],L,Ind, [cloudy(Ind) | C],C).
adj(["rainy" | L],L,Ind, [rainy(Ind) | C],C).
adj(["sunny" | L],L,Ind, [sunny(Ind) | C],C).

adj_to_noun("cold", T, "temperature") :- temperature(T).
adj_to_noun("warm", T, "temperature") :- temperature(T).
adj_to_noun("windy", S,  "wind speed") :- wind_speed(S).
adj_to_noun("humid", H, "humidity") :- humidity(H).
adj_to_noun("cloudy", C, "cloud cover") :- cloud(C).
adj_to_noun("snowy", S, "chance of snow") :- chance_of_snow(S).
adj_to_noun("rainy", R, "chance of rain") :- chance_of_rain(R).
adj_to_noun("sunny", C, "cloud cover") :- cloud(C).

noun(["temperature" | L],L,Ind, [temperature(Ind) | C],C).
noun(["wind", "speed" | L],L,Ind, [wind_speed(Ind) | C],C).
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

% personal items
noun(["umbrella" | L], L, Ind, [umbrella(Ind) | C], C).
noun(["coat" | L], L, Ind, [coat(Ind) | C], C).
noun(["raincoat" | L], L, Ind, [raincoat(Ind) | C], C).
noun(["sunglasses" | L], L, Ind, [sunny(Ind) | C], C).

verb_phrase(["is", "it" | L], L, _, C, C).

pronoun(["it" |L], L, _, C, C).

reln(["in" | L],L,Ind,Ind,C,C).
reln(["the", "temperature", "in" | L],L,Sub,_, [temperature(Sub)|C],C).
reln(["the", "wind", "speed", "in" | L],L,Sub,_, [wind_speed(Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,_, [humidity(Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,_, [humidity(Sub)|C],C).
reln(["the", "pressure", "in" | L],L,Sub,_, [pressure(Sub)|C],C).
reln(["the", "wind", "chill", "in" | L],L,Sub,_, [feels_like(Sub)|C],C).

% ---------------- Asking questions ----------------
% a question can be an auxiliary verb followed by subject and then an adjective
% - Is Vancouver cold?
% - Is Vancouver warm?
% - Is Vancouver windy?
question(["is" | L0],L3,Ind,C0,C3) :-
    aphrase(L0,L1, Ind, C0,C1),
    adj(L1,L2,Ind,C1,C2),
    omp(L2, L3, Ind, C2, C3).

%question(["is" | L0], L2, Ind, C0, C2) :-
%    aphrase(L0,L1, Ind, C0,C1),
%    verb(L1,L2,Ind,C1,C2).

question(["what", "is" | L0],L1,Ind,C0,C1) :-
    aphrase(L0,L1,Ind,C0,C1).
question(["do", "i", "need" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).
question(["should", "i", "bring" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).
question(["should", "i", "wear" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).

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


:- discontiguous prove_all/4.
% prove_all(L) is true if all elements of L can be proved from the knowledge base

prove_all(["is"| _],[Last],_, _) :-
    \+ call(Last),
    adj([Adj | _],_,_, [Last | _],_),
    atom_concat("No, it is not ", Adj, PrintAns1),
    atom_concat(PrintAns1, "\n", PrintAns),
    write(PrintAns),
    flush_output(current_output),
    adj_to_noun(Adj, Val, Noun),
    noun([Noun | _], _, _, [Constraint | _], _),
    explanation(Noun, Constraint, Val, PrintExp),
    write(PrintExp), flush_output(current_output),
    fail.

prove_all(["is"| _],[Last],_, _) :-
    call(Last),
    adj([Adj | _],_,_, [Last | _],_),
    atom_concat("Yes, it is ", Adj, PrintAns1),
    atom_concat(PrintAns1, "\n", PrintAns),
    write(PrintAns), flush_output(current_output),
    adj_to_noun(Adj, Val, Noun),
    noun([Noun | _], _, _, [Constraint | _], _),
    explanation(Noun, Constraint, Val, PrintExp),
    write(PrintExp), flush_output(current_output).

accessory_questions(Last,  DoOrShould, BringOrWear) :-
    call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    format(atom(PrintAns), "Yes, you ~s ~s a(n) ~s. ", [DoOrShould, BringOrWear, Noun]),
    write(PrintAns),
    flush_output(current_output),
    get_explanation(Noun, _, Exp),
    write(Exp), flush_output(current_output).

accessory_questions(Last, DoOrShould, BringOrWear) :-
    \+ call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    format(atom(PrintAns), "No, you ~s not ~s a(n) ~s. ", [DoOrShould, BringOrWear, Noun]),
    write(PrintAns),
    flush_output(current_output),
    get_explanation(Noun, _, Exp),
    write(Exp), flush_output(current_output),
    fail.

prove_all(["should", "i", "bring" | _], [Last],_, _) :-
    accessory_questions(Last, "should", "bring").

prove_all(["should", "i", "wear" | _], [Last],_, _) :-
    accessory_questions(Last, "should", "wear").

prove_all(["do", "i", "need" | _], [Last],_, _) :-
    accessory_questions(Last, "do", "need").

%prove_all(["Is"| _],[H|T],_, _) :-
%    call(H),      % built-in Prolog predicate calls an atom
%    prove_all(["Is"| _], T, _, _), !.

prove_all(["what"| _],[Last],_, Ind) :-
    call(Last),
    noun([Noun | _],_,_, [Last | _],_),
    explanation(Noun, Last, Ind, PrintAns),
    write(PrintAns), flush_output(current_output).

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


% To get the input from a line:
%q(Ans) :-
%    write("What city would like to ask about? "), flush_output(current_output),
%    read_line_to_string(user_input, CitySt),
%    add_facts_to_database(CitySt),
%    atom_concat(CitySt, "? ", CityQ),
%    atom_concat("Great! What would you like to know about ", CityQ, PrintPrompt),
%    write(PrintPrompt), flush_output(current_output),
%    read_line_to_string(user_input, St),
%    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
%    ask(Ln, Ans).
q(Ans) :-
    write("What city would you like to ask about? "), flush_output(current_output),
    read_line_to_string(user_input, CitySt),
    (add_facts_to_database(CitySt)
    ->  atom_concat(CitySt, "? ", CityQ),
        atom_concat("Great! What weather facts would you like to know about ", CityQ, PrintPrompt),
        write(PrintPrompt), flush_output(current_output),
        ask_questions(Ans)
    ;   format("We could not find weather data for ~w. Please try again.", [CitySt]), Ans =
    []).
%q(Ans) :-
%    write("No answers\n"),
%    q(Ans).

%ask_questions(Ans) :-
%    read_line_to_string(user_input, St),
%    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
%    \+ ask(Ln, Ans),
%    write("\nDo you have more questions? (y/n) \n"), flush_output(current_output),
%    read_line_to_string(user_input, YesOrNo),
%    (YesOrNo == "y" -> ask_questions(Ans) ; fail).

ask_questions(Ans) :-
    read_line_to_string(user_input, StIn),
    downcase_atom(StIn, St),
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    (catch(ask(Ln, Ans), _, fail) ->
        write("No answers")
        ;
        true),
    write("\nDo you have more questions? (y/n) \n"), flush_output(current_output),
    read_line_to_string(user_input, YesOrNoIn),
    downcase_atom(YesOrNoIn, YesOrNo),
    (YesOrNo == "y" -> write("Ask away!\n"), ask_questions(Ans) ; fail).

% ---------------- Predicates about weather ----------------
% ----- Interpretations about the weather to answer questions -----
% cold(C) is true if the temperature is T and it is below 10 degrees.
cold(T) :- temperature(T), T =< 10.0.

% warm(C) is true if the temperature is T and it is above 10 degrees
warm(T) :- temperature(T), T > 10.0.

windy(S) :- wind_speed(S), S > 10.0.

rainy(P) :- chance_of_rain(P), P > 50.

humid(H) :- humidity(H), H > 60.

cloudy(C) :- cloud(C), C > 60.

sunny(C) :- cloud(C), C < 60.

coat(C) :- cold(C).

umbrella(C) :- rainy(C).

raincoat(C) :- rainy(C).




