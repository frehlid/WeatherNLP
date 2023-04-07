/* 
This file is based on geography_query_string.pl from cpsc312
sample NLP parsing of a simple weather question
	- "What is the temperature in Vancouver?"

We want to try and parse the question into a form that we can use to query an API.
We will use a simple grammar to parse the question.
*/

% A noun phrase is a determiner followed by adjectives followed by a noun and an optinal modifier phrase.
noun_phrase(L0, L4, Ind, C0, C4) :-
	det(L0, L1, Ind, C0, C1),
	adjectives(L1, L2, Ind, C1, C2),
	noun(L2, L3, Ind, C2, C3),
	omp(L3, L4, Ind, C3, C4).


% the is a determiner
det(["the" | L],L,_,C,C).
det(L,L,_,C,C).

% the noun temperature imposes the constraint that the subject of the query is temperature
noun(["temperature" | L], L, Ind, [subject(temperature)|C], C).
noun(["wind", "speed" | L], L, Ind, [subject(wind_speed)|C], C).
noun(["vancouver" | L], L, Ind, [city(vancouver)|C], C).

% currently no adjectives
adjectives(L,L,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(L0,L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).

% An optional modifying phrase is either a modifying phrase or nothing
omp(L0,L1,E,C0,C1) :-
    mp(L0,L1,E,C0,C1).
omp(L,L,_,C,C).

reln(["in" | L], L, _, _, C, C).

% get_constraints_from_question(Q,A,C) is true if C is the constraints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    question(Q,End,A,C,[]),
    member(End,[[],["?"],["."]]). % make sure we have reached the end of the question

% A queston starting with "What is" can be a noun phrase.
question(["What", "is" | L0], L1, Ind, C0, C1) :-
	noun_phrase(L0, L1, Ind, C0, C1).

ask(Q,A,C) :- get_constraints_from_question(Q,A,C), query(C).

%--- database of possible queries --- %
% query(Constriants) is true if there is a query that can be made with the given constraints
% constraints are of the form [subject(S), city(Cty)]
query(C) :-
	member(subject(S), C),
	member(city(Cty), C).

% a subject is a question to ask an API
% temperature is a subject
subject(temperature).
subject(wind_speed).

% vancouver is a city
city(vancouver).



