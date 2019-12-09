% Prolog representation of a grammar to ask a query of a database
% Builds a query which can then be asked of the knowledge base
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is code adapted from Figure 13.12 in Section 13.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2017

% Adapted from code by David Poole and Alan Mackworth. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(L0,L4,Entity,C0,C4) is true if
%  L0 and L4 are list of words, such that
%        L4 is an ending of L0
%        the words in L0 before L4 (written L0-L4) form a noun phrase
%  Entity is an individual that the noun phrase is referring to
% C0 is a list such that C4 is an ending of C0 and C0-C4 contains the constraints imposed by the noun phrase

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,Entity,C0,C4,Memory) :-
    det(L0,L1,Entity,C0,C1,Memory),
    adjectives(L1,L2,Entity,C1,C2,Memory),
    noun(L2,L3,Entity,C2,C3,Memory),
    mp(L3,L4,Entity,C3,C4,Memory).
noun_phrase(L0,L4,Entity,C0,C4,Memory) :-
    proper_noun(L0,L4,Entity,C0,C4,Memory).    

% Try:
%?- noun_phrase([a,spanish,speaking,country],L1,E1,C0,C1).
%?- noun_phrase([a,country,that,borders,chile],L1,E1,C0,C1).
%?- noun_phrase([a,spanish,speaking,country,that,borders,chile],L1,E1,C0,C1).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | L],L,_,C,C,_).
det([a | L],L,_,C,C,_).
det(L,L,_,C,C,_).


% adjectives(L0,L2,Entity,C0,C2) is true if 
% L0-L2 is a sequence of adjectives imposes constraints C0-C2 on Entity
adjectives(L0,L2,Entity,C0,C2,Memory) :-
    adj(L0,L1,Entity,C0,C1,Memory),
    adjectives(L1,L2,Entity,C1,C2,Memory).
adjectives(L,L,_,C,C,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(L0,L2,Subject,C0,C2,Memory) :-
    reln(L0,L1,Subject,Object,C0,C1,Memory),
    noun_phrase(L1,L2,Object,C1,C2,Memory).
mp([that|L0],L2,Subject,C0,C2,Memory) :-
    reln(L0,L1,Subject,Object,C0,C1,Memory),
    noun_phrase(L1,L2,Object,C1,C2,Memory).
mp([that|L0],L2,Subject,C0,C2,Memory) :-
    reln(L0,L2,Subject,_,C0,C2,Memory).
mp([that|L0],L2,Subject,C0,C2,Memory) :-
    reln(L0,L1,Subject,_,C0,C1,Memory),
    mp(L1,L2,Subject,C1,C2,Memory).   
mp(L,L,_,C,C,_).

% DICTIONARY
% adj(L0,L1,Entity,C0,C1) is true if L0-L1 
% is an adjective that imposes constraints C0-C1 Entity
adj([large | L],L,Entity, [large(Entity)|C],C,_).
adj([new | L],L,Entity, [new(Entity,Memory)|C],C,Memory).
adj([recommended | L],L,Entity, [minmemlikes(Entity,Memory)|C],C,Memory).
adj([Lang,speaking | L],L,Entity, [speaks(Entity,Lang)|C],C,_).
adj([Lang,-,speaking | L],L,Entity, [speaks(Entity,Lang)|C],C,_).

noun([country | L],L,Entity, [country(Entity)|C],C,_).
noun([continent | L],L,Entity, [continent(Entity)|C],C,_).
noun([city | L],L,Entity, [city(Entity)|C],C,_).
noun([island | L],L,Entity, [island(Entity)|C],C,_).

% Countries and languages are proper nouns.
% We could either have it check a language dictionary or add the constraints. We chose to check the dictionary.
proper_noun([X | L],L,X,C,C,_) :- country(X).
proper_noun([X | L],L,X,C,C,_) :- continent(X).
proper_noun([X | L],L,X,C,C,_) :- language(X).
proper_noun([X | L],L,X,C,C,_) :- attraction(X).

reln([borders | L],L,O1,O2,[borders(O1,O2)|C],C,_).
reln([the,capital,of | L],L,O1,O2, [capital(O2,O1)|C],C,_).
reln([next,to | L],L,O1,O2, [borders(O1,O2)|C],C,_).
reln([in | L],L,O1,O2, [in_continent(O1,O2)|C],C,_).
reln([is,near | L],L,O1,O2, [same_continent(O1,O2)|C],C,_).
reln([near | L],L,O1,O2, [same_continent(O1,O2)|C],C,_).
reln([has | L],L,O1,O2, [has(O1,O2)|C],C,_).

reln(['I',might,like| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',might,enjoy| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',might,love| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',would,like| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',would,enjoy| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',would,love| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['you',would,recommend| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['you',might,recommend| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',will,like| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',will,enjoy| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).
reln(['I',will,love| L],L,O1,_, [minmemlikes(O1,Memory)|C],C,Memory).

% question(Question,QR,Entity,Memory) is true if Query provides an answer about Entity to Question
question(['Is' | L0],L2,Entity,C0,C2,Memory) :-
    noun_phrase(L0,L1,Entity,C0,C1,Memory),
    mp(L1,L2,Entity,C1,C2,Memory).
question(['What',is | L0], L1, Entity,C0,C1,Memory) :-
    mp(L0,L1,Entity,C0,C1,Memory).
question(['What',is | L0],L1,Entity,C0,C1,Memory) :-
    noun_phrase(L0,L1,Entity,C0,C1,Memory).
question(['What' | L0],L2,Entity,C0,C2,Memory) :-
    noun_phrase(L0,L1,Entity,C0,C1,Memory),
    mp(L1,L2,Entity,C1,C2,Memory).

% TEST ask(['What',is,a,country,near,brazil,that,I,might,like,'?'],A,[birdwatching,winter]).    

% ask(Q,A,Memory) gives answer A to question Q, possibly constrained by facts about the user stored in memory
ask(Q,A,Memory) :-
    get_constraints_from_question(Q,A,C,Memory),
    prove_all(C).

% get_constraints_from_question(Q,A,C,Memory) is true if C is the constaints on A to infer question Q
get_constraints_from_question(Q,A,C,Memory) :-
    question(Q,End,A,C,[],Memory),
    member(End,[[],['?'],['.']]).

% prove_all(L) is true if all elements of L can be proved from the knowledge base
prove_all([]).
prove_all([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(T).

% minmemlikes(Entity,Memory) is true if memory shows there is at least one reason to like Entity
minmemlikes(Entity,Memory):-
    collectlikes(Entity,Memory,Likes),
    length(Likes,Count),
    Count >= 1.
    
collectlikes(Entity,Memory,Result):-
    include(addlike(Entity),Memory,Result).    

addlike(Entity,X):-
    country(Entity),
    continent(X),
    in_continent(Entity,X).
addlike(Entity,X):-
    country(Entity),
    language(X),
    speaks(Entity,X).
addlike(Entity,X):-
    country(Entity),
    attraction(X),
    has(Entity,X).

new(Entity,Memory):-
    country(Entity),
    \+ member(Entity,Memory).
    
new(Entity,Memory):-
    continent(Entity),
    \+ member(Entity,Memory).
    
%  The Database of Facts to be Queried

% country(C) is true if C is a country
country(argentina).
country(brazil).
country(chile).
country(paraguay).
country(peru).
country(russia).
country(france).
country(spain).
country(china).
country(japan).

% continent(C) is true if C is a continent
continent(south_america).
continent(europe).
continent(asia).

% large(C) is true if the area of C is greater than 2million km^2
large(brazil).
large(argentina).
large(russia).
large(china).

% island(C) is true if C is an island nation
island(japan).   

% language(L) is true if L is a language
language(spanish).
language(portugese).
language(russian).
language(french).
language(chinese).
language(japanese).

% same_continent(C1,C2) is true if C1 and C2 are on the same continent
same_continent(C1,C2) :- 
    in_continent(C1,Continent),
    in_continent(C2,Continent),
    dif(C1,C2).

% in_continent(C,Cont) is true if C has territory in Cont
in_continent(argentina,south_america).
in_continent(brazil,south_america).
in_continent(chile,south_america).
in_continent(paraguay,south_america).
in_continent(peru,south_america).
in_continent(russia,europe).
in_continent(russia,asia).
in_continent(france,europe).
in_continent(spain,europe).
in_continent(china,asia).
in_continent(japan,asia).

% speaks(Country,Lang) is true of Lang is an official language of Country
speaks(argentina,spanish).
speaks(brazil,portugese).
speaks(chile,spanish).
speaks(paraguay,spanish).
speaks(peru,spanish).
speaks(russia,russian).
speaks(spain,spanish).
speaks(france,french).
speaks(china,chinese).
speaks(japan,japanese).

% capital(C1,C2) is true if C2 is the capital city of country C1
capital(argentina,'Buenos Aires').
capital(chile,'Santiago').
capital(peru,'Lima').
capital(brazil,'Brasilia').
capital(paraguay,'Asunción').
capital(russia,'Moscow').
capital(spain,'Madrid').
capital(france,'Paris').
capital(china,'Beijing').
capital(china,'Tokyo').

% borders(C1,C2) is true if country C1 borders country C2
borders(peru,chile).
borders(chile,peru).
borders(argentina,chile).
borders(chile,argentina).
borders(brazil,peru).
borders(peru,brazil).
borders(argentina,brazil).
borders(brazil,argentina).
borders(brazil,paraguay).
borders(paraguay,brazil).
borders(argentina,paraguay).
borders(paraguay,argentina).
borders(france,spain).
borders(spain,france).
borders(russia,china).
borders(china,russia).

% attraction(A) is true if A is an attraction that a location could have
attraction(rainforests).
attraction(birdwatching).
attraction(alcohol).
attraction(drinking).
attraction(skiing).
attraction(football).
attraction(badminton).
attraction(crying).
attraction(chocolate).
attraction(llamas).
attraction(walls).
attraction(towers).
attraction(suntanning).
attraction(winter).
attraction(trains).
attraction(eating).
attraction(food).
attraction(hiking).

% has(C,I) is true if country C has attraction I, continent C has I, or country C's parent continent has I
has(Country,Item) :- 
    country(Country),
    continent(Continent),
    in_continent(Country, Continent),
    has(Continent,Item).

has(south_america,rainforests).
has(south_america,birdwatching).
has(europe,alcohol).
has(europe,drinking).
has(europe,skiing).
has(south_america,football).
has(europe,football).
has(asia,badminton).
has(south_america,eating).
has(europe,eating).
has(asia,eating).
has(south_america,food).
has(europe,food).
has(asia,food).

has(russia, crying).
has(argentina, hiking).
has(chile, hiking).
has(china, hiking).
has(japan, hiking).
has(brazil, chocolate).
has(peru, llamas).
has(china, walls).
has(france, towers).
has(spain, suntanning).
has(chile, winter).
has(russia, winter).
has(france, winter).
has(japan, winter).
has(china, winter).
has(china, trains).
has(japan, trains).

/* Try the following queries:
?- ask(['What',is,a,country],A).
?- ask(['What',is,a,spanish,speaking,country],A).
?- ask(['What',is,the,capital,of, chile],A).
?- ask(['What',is,the,capital,of, a, country],A).
?- ask(['What',is, a, country, that, borders,chile],A).
?- ask(['What',is, a, country, that, borders,a, country,that,borders,chile],A).
?- ask(['What',is,the,capital,of, a, country, that, borders,chile],A).
?- ask(['What',country,borders,chile],A).
?- ask(['What',country,that,borders,chile,borders,paraguay],A).
*/


% To get the input from a line:

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    ask(Ln,Ans,[]).
    
% qRus(Ans) :-
%    write("Ask me: "), flush_output(current_output),
%    readln(Ln),
%    ask(Ln,Ans,[skiing,winter,alcohol,crying]).
%    ask(['What',is,a,country,that,'I',might,like,'?'],Ans,[skiing,winter,alcohol,crying]).    
   

/*
?- q(Ans).
Ask me: What is a country that borders chile?
Ans = argentina ;
Ans = peru ;
false.

?- q(Ans).
Ask me: What is the capital of a spanish speaking country that borders argentina?
Ans = 'Santiago' ;
Ans = 'Asunción' ;
false.

Some more questions:
What is next to chile?
Is brazil next to peru?
What is a country that borders a country that borders chile.
What is borders chile?
What borders chile?
What country borders chile?
What country that borders chile is next to paraguay?
What country that borders chile next to paraguay?

What country borders chile?
What country that borders chile is next to paraguay?
What country that borders chile next to paraguay?
*/
