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


noun(["temperature" | L],L,Ind, [temperature(Ind) | C],C).
noun(["wind", "speed" | L],L,Ind, [wind_speed(Ind) | C],C).
noun(["humidity" | L],L,Ind, [humidity(_,Ind) | C],C).
noun(["pressure" | L],L,Ind, [pressure(_,Ind) | C],C).
noun(["wind", "chill" | L],L,Ind, [feels_like(_,Ind) | C],C).

noun(["Vancouver" |L], L, _, C, C).

% personal items
noun(["umbrella" | L], L, umbrella, C, C).
noun(["coat" | L], L, coat, C, C).

%noun(["umbrella" | L],L,Ind, [umbrella(Ind) | C],C).

reln(["in" | L],L,Ind,Ind,C,C).
reln(["the", "temperature", "in" | L],L,Sub,_, [temperature(Sub)|C],C).
reln(["the", "wind", "speed", "in" | L],L,Sub,_, [wind_speed(Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,Obj, [humidity(Obj,Sub)|C],C).
reln(["the", "humidity", "in" | L],L,Sub,Obj, [humidity(Obj,Sub)|C],C).
reln(["the", "pressure", "in" | L],L,Sub,Obj, [pressure(Obj,Sub)|C],C).
reln(["the", "wind", "chill", "in" | L],L,Sub,Obj, [feels_like(Obj,Sub)|C],C).

% ---------------- Asking questions ----------------
% a question can be an auxiliary verb followed by subject and then an adjective
% - Is Vancouver cold?
% - Is Vancouver warm?
% - Is Vancouver windy?
question(["Is" | L0],L3,Ind,C0,C3) :-
    aphrase(L0,L1, Ind, C0,C1),
    adj(L1,L2,Ind,C1,C2),
    omp(L2, L3, Ind, C2, C3).

%changed this to call aphrase, in case subject is not city
question(["What", "is" | L0],L1,Ind,C0,C1) :-
    aphrase(L0,L1,Ind,C0,C1).

%question(["Do", "I", "need" | L0],L1,Ind,C0,C1).

question(["Should", "I", "wear" | L0],L1,Ind,C0,C1) :-
    noun_phrase(L0, L1, Ind, C0, C1).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    get_constraints_from_question(Q,Ind,C),
    prove_all(Q,C, A, Ind), !.

% get_constraints_from_question(Q,A,C) is true if C is the constraints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    question(Q,End,A,C,[]),
    member(End,[[],["?"],["."]]). % check that the question ends with a period


% prove_all(L) is true if all elements of L can be proved from the knowledge base
prove_all(_,[],_, _).
prove_all(["Is"| _],[H|T],_, _) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(["Is"| _], T, _, _).


prove_all(_,[],_, _).
prove_all(_,[H|T],A, Ind) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(_, T, A, Ind),
    A is Ind.

% To get the input from a line:
q(Ans) :-
    add_facts_to_database(_),
    write("Ask me: "), flush_output(current_output),
    read_line_to_string(user_input, St),
    split_string(St, " -", " ,?.!-", Ln), % ignore punctuation
    ask(Ln, Ans).
q(Ans) :-
    write("No more answers\n"),
    q(Ans).

% ---------------- Predicates about weather ----------------
% ----- Interpretations about the weather to answer questions -----
% cold(C) is true if the temperature is T and it is below 10 degrees.
cold(_) :- temperature(T), T =< 10.0.

% warm(C) is true if the temperature is T and it is above 10 degrees
warm(_) :- temperature(T), T > 10.

windy(_) :- wind_speed(S), S > 10.

% humid(C) is true if the humidity in C is above 50%
humid(H) :- humidity(_, H), H > 50.

% umbrella(C) is true if the chance of rain is above
umbrella(R) :- chance_rain(_, R), R > 50.


