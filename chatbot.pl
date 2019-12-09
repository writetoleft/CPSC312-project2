:- ensure_loaded('questions.pl').

% start program
hello() :-
    write("What is your name?"), nl(), flush_output(current_output),
    readln(NameList),
    atomic_list_concat(NameList, ' ', Name),
    write("Ask me anything about places to go to, or tell me things you like, so we can plan a great trip: "), flush_output(current_output),
    readln(Ln),
    chatbot(Ln, [val('Name', Name)]).

% deprecated 
clarify() :-
    write("Could you rephrase that for me please?"), nl(), flush_output(current_output),
    readln(Ln),
    chatbot(Ln, _).

% alternative lines for exiting program
chatbot(['bye'|_], Memory) :- bye(Memory).
chatbot(['Bye'|_], Memory) :- bye(Memory).
chatbot(['goodbye'|_], Memory) :- bye(Memory).
chatbot(['Goodbye'|_], Memory) :- bye(Memory).
chatbot(['cya'|_], Memory) :- bye(Memory).
chatbot(['Cya'|_], Memory) :- bye(Memory).
chatbot(['ttyl'|_], Memory) :- bye(Memory).
chatbot(['Ttyl'|_], Memory) :- bye(Memory).

% prints Memory to chat
chatbot(['print'], Memory) :- 
    write("This is what I remember about our conversations:"), nl(), 
    write(Memory), nl(),
    write("What else do you like? Or do you want to ask me about places?"), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, Memory).
    
% read line, succeeds when user types in rude phrases 
chatbot(Ln, Memory) :-
    rude_phrase(Ln, P),
    string_concat(P, " is a strong word - not a nice thing to be saying ", Reply),
    member(val('Name', Name), Memory),
    string_concat(Reply, Name, Reply2),
    string_concat(Reply2, "!", ReplyL),
    write(ReplyL), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, Memory). 

% read line, succeeds when user is asserting they like something but not asking question
chatbot(Ln, Memory) :-
    % write('Like phrase case'), nl(), flush_output(current_output),
    like_phrase(Ln, Entity),
    \+ like_question(Ln),
    dif(Entity, ''),
    member(val('Name', Name), Memory),
    string_concat("I understand that you like ", Entity, Reply),
    string_concat(Reply, ", ", Reply2),
    string_concat(Reply2, Name, ReplyL),
    write(ReplyL), nl(), flush_output(current_output),
    write("What else do you like? Or do you want to ask me about places?"), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, [Entity|Memory]).

% read line, successfully resolves question   
chatbot(Ln, Memory) :-
    % write('Question case'), nl(), flush_output(current_output),
    % write(Ln), nl(), flush_output(current_output),
    \+ like_phrase(Ln, _),
    ask(Ln, Ans, Memory),
    % write(Ln), nl(), flush_output(current_output),
    write(Ans), nl(), flush_output(current_output),
    write('Any other questions? Or would you like to tell me a bit about things you like?'), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, [Ans|Memory]).     

% read line, not enough likes in memory to query against  
chatbot(Ln, Memory) :-
    % write('Not enough likes case'), nl(), flush_output(current_output),
    % write(Ln), nl(), flush_output(current_output),
    \+ like_phrase(Ln, _),
    like_question(Ln),
    \+ ask(Ln,_,_),
    % write(Ln), nl(), flush_output(current_output),
    write('I don\'t know enough about what you like to recommend anything. Give me more examples.'), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, Memory).      

% read line, fail to resolve question or statement    
chatbot(Ln, Memory) :-
    % write('Misunderstood case'), nl(), flush_output(current_output),
    % write(Ln), nl(), flush_output(current_output),
    \+ ask(Ln,_,_),
    % write(Ln), nl(), flush_output(current_output),
    write('Im not sure I have an answer for that. Could you ask something else?'), nl(), flush_output(current_output),
    readln(Ln2),
    chatbot(Ln2, Memory). 

% checks input list for yes or no strings   
check_yesno(['yes'|_], yes).
check_yesno(['Yes'|_], yes).
check_yesno(['ya'|_], yes).
check_yesno(['Ya'|_], yes).
check_yesno(['yup'|_], yes).
check_yesno(['Yup'|_], yes).
check_yesno(['no'|_], nah).
check_yesno(['No'|_], nah).
check_yesno(['nope'|_], nah).
check_yesno(['Nope'|_], nah).
check_yesno(['nah'|_], nah).
check_yesno(['Nah'|_], nah).

% exits program
bye(Memory) :-
    member(val('Name', Name), Memory),
    string_concat('Bye ', Name, ByeMessage),
    string_concat(ByeMessage, ', talk to you again soon!', ByeMessage2),
    write(ByeMessage2), flush_output(current_output).

% attempts to match input against rude phrases
rude_phrase(['hate'|_], 'Hate').
rude_phrase(['stupid'|_], 'Stupid').
rude_phrase(['annoying'|_], 'Annoying').
rude_phrase(['ugly'|_], 'Ugly').
rude_phrase([_|T],X) :-
    rude_phrase(T,X).

% attempts to match input against ways to end question indicating user preference    
like_question(['like','?']).
like_question(['enjoy','?']).
like_question(['recommend','?']).
like_question(['love','?']).
like_question([_|T]) :-
    like_question(T).

% attempts to match input against statements about what user likes 
like_phrase(['like','to',E|_], Entiting) :-
    convert_infinitive(E, Entiting),
    dif(Entiting,'?').    
like_phrase(['enjoy','to',E|_], Entiting) :-
    convert_infinitive(E, Entiting),
    dif(Entiting,'?').
like_phrase(['love','to',E|_], Entiting) :-
    convert_infinitive(E, Entiting),
    dif(Entiting,'?').
like_phrase(['like',Entity|_], Entity) :-
    dif(Entity, 'to'),
    dif(Entity,'?').
like_phrase(['enjoy',Entity|_], Entity) :-
    dif(Entity, 'to'),
    dif(Entity,'?').
like_phrase(['love',Entity|_], Entity) :-
    dif(Entity, 'to'),
    dif(Entity,'?').
like_phrase([_|T],X) :-
    like_phrase(T,X).

% converts verb forms to nouns    
convert_infinitive(S, SR) :-
    sub_string(S, _, 1, 0, 'e'),
    sub_string(S,_,1,1,'e'),
    string_concat(S,'ing', SR).
convert_infinitive(S, SR) :-
    sub_string(S, _, 1, 0, 'e'),
    sub_string(S,_,1,1,'i'),
    sub_string(S,0,_,2,SubstringL),
    string_concat(SubstringL,'ying', SR).
convert_infinitive(S, SR) :-
    sub_string(S, _, 1, 0, 'e'),
    sub_string(S,_,1,1,SubstringX),
    dif(SubstringX, 'i'),
    dif(SubstringX, 'e'),
    sub_string(S,0,_,1,SubstringL),
    string_concat(SubstringL,'ing', SR).
convert_infinitive(S, SR) :-
    sub_string(S, _, 1, 0, Substring),
    dif(Substring, 'e'),
    string_concat(S,'ing', SR).

