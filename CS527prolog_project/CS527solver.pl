:- use_module(library(clpfd)).
:- use_module(library(lists)).
    
%general solver predicate 
solve(Start,Goal,Problem,Search):- (Problem == vamp-wolf ->
					 (Search == dfs -> goDfs(Start,Goal);
					  Search == bfs -> goBfs(Start,Goal);
					  Search == hfs -> go(Start,Goal,Problem));
					 Problem == sliding-tile ->
					 (Search == dfs -> goDfs(Start,Goal);
					  Search == bfs -> goBfs(Start,Goal);
					  Search == hfs -> go(Start,Goal,Problem))).
    
%vamp-wolf moves
move(state(X,V,W),state(Y,NV,W)):- X == east -> NV is (V-1), Y = west; NV is (V+1), Y = east.
move(state(X,V,W),state(Y,NV,W)):- X == east ->NV is (V-2), Y = west; NV is (V+2), Y = east.
move(state(X,V,W),state(Y,V,NW)):- X == east ->NW is (W-1), Y = west; NW is (W+1), Y = east.
move(state(X,V,W),state(Y,V,NW)):- X == east ->NW is (W-2), Y = west; NW is (W+2), Y = east.
move(state(X,V,W),state(Y,NV,NW)):- X == east -> NV is (V-1),
    NW is (W-1), Y = west; NV is (V+1), NW is (W+1), Y = east.
    
%sliding-tile moves
move(state(L1),state(L2)):- member(0,L1),
    indexOf(L1,0,Index),Index1 is Index - 4, Index1 >=0,
    nth0(Index1,L1,Num),replace(L1,Index,Num,Temp),
    replace(Temp,Index1,0,L2).
move(state(L1),state(L2)):- member(0,L1),
    indexOf(L1,0,Index),Index1 is Index + 4,Index1 =< 15,
    nth0(Index1,L1,Num),replace(L1,Index,Num,Temp),
    replace(Temp,Index1,0,L2).
    
move(state(L1),state(L2)):- member(0,L1),
    indexOf(L1,0,Index),Index1 is Index - 1,Index1 >= 0,
    nth0(Index1,L1,Num),replace(L1,Index,Num,Temp),
    replace(Temp,Index1,0,L2).
    
move(state(L1),state(L2)):- member(0,L1),
    indexOf(L1,0,Index),Index1 is Index + 1,Index1 =< 15,
    nth0(Index1,L1,Num),replace(L1,Index,Num,Temp),
    replace(Temp,Index1,0,L2).

%vamp-wolf safe state    
isSafe(state(_,V,W)) :- not(W>V), isValid(_,V,W).
    
%sliding-tile state  
isSafe(state(List)):- length(List,Num),Num ==16 ,all_different(List),
    maplist(=<(0),List),
    maplist(>=(15),List),
    foldl(plus, List, 0, Result),
    Result ==120.
    
%valid state for vamp-wolf
isValid(_,V,W) :- V >= 0, V =< 3, W >= 0, W =< 3.


%heuristic vamp-wolf, try absolute value( goal - current state) to find value of difference of the states
heuristic_vw(state(_,V,W),state(_,V1,W1),H):- H is abs((V1+W1)-(V+W))/2.

%heuristic sliding-tile, root sum of square difference eq. 
heuristic_st(state(L1),state(L2),H):- maplist((sub),L1,L2,Temp),maplist((square),Temp,Temp2),foldl(plus,Temp2,0,Temp3),sqrt(Temp3,H).

/*credit: replace function to stack overflow post for replace implementation, I was really struggling with the recursion in Prolog
I was trying to write a function to swap the elements from as one index to another, but am a little confused by the pattern
matching. Its a little different from Haskell and couldnt quite figure it out. */ 
%https://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

/*credit: indexOf function came from stack overflow post. Still trying to look in list library 
 to replce this function, but cant find anything so far. The recursion is easier to follow here, 
 than in replace*/
%https://stackoverflow.com/questions/4380624/how-compute-index-of-element-in-a-list
    
indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

square(X,Y):-Y is X*X.
sub(X,Y,Z):- Z is X-Y.
    
head([A|_],B):-B = A.

    
%%%%% Basic depth first path algorithm in PROLOG %%%%%%%
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

	
goDfs(Start, Goal) :-
	empty_stack(Empty_been_list),
	stack(Start, Empty_been_list, Been_list),
	pathDfs(Start, Goal, Been_list).
	
	% path implements a depth first search in PROLOG
	
	% Current state = goal, print out been list
pathDfs(Goal, Goal, Been_list) :-
        write('Solution path is:'),nl,
        reverse_print_stack(Been_list).
	
pathDfs(State, Goal, Been_list) :-
	move(State, Next),
    %   not(unsafe(Next)),
        isSafe(Next),
	not(member_stack(Next, Been_list)),
        stack(Next, Been_list, New_been_list),
        write('Visited:'),write(Next),nl,
	pathDfs(Next, Goal, New_been_list), !.
	
reverse_print_stack(S) :-
	empty_stack(S).
reverse_print_stack(S) :-
	stack(E, Rest, S),
	reverse_print_stack(Rest),
	write(E), nl.


%%%%%%% Breadth first search algorithm%%%%%%%%
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

    
state_record_bfs(State, Parent, [State, Parent]).

goBfs(Start, Goal) :- 
    empty_queue(Empty_open),
    state_record_bfs(Start, nil, State),
    add_to_queue(State, Empty_open, Open),
    empty_set(Closed),
    pathBfs(Open, Closed, Goal).

pathBfs(Open,_,_) :- empty_queue(Open),
                  write('graph searched, no solution found').
    
pathBfs(Open, Closed, Goal) :- 
    remove_from_queue(Next_record, Open, _),
    state_record_bfs(State, _, Next_record),
    State = Goal,
    write('Visited:'),write(State),nl,
    write('Solution path is: '), nl,
    printsolutionBfs(Next_record, Closed).
    
pathBfs(Open, Closed, Goal) :- 
    remove_from_queue(Next_record, Open, Rest_of_open),
    head(Next_record,Head),write('Visited:'), write(Head),nl,
    (bagof(Child, movesBfs(Next_record, Open, Closed, Child), Children);Children = []),
    add_list_to_queue(Children, Rest_of_open, New_open), 
    add_to_set(Next_record, Closed, New_closed),
    pathBfs(New_open, New_closed, Goal),!.

movesBfs(State_record_bfs, Open, Closed, Child_record) :-
    state_record_bfs(State, _, State_record_bfs),
    move(State, Next),
    % not (unsafe(Next)),
    isSafe(Next),
    state_record_bfs(Next, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record_bfs(Next, State, Child_record).

printsolutionBfs(State_record_bfs, _):- 
    state_record_bfs(State,nil, State_record_bfs),
    write(State), nl.
printsolutionBfs(State_record_bfs, Closed) :-
    state_record_bfs(State, Parent, State_record_bfs),
    state_record_bfs(Parent, _, Parent_record),
    member(Parent_record, Closed),
    printsolutionBfs(Parent_record, Closed),
    write(State), nl.
        
add_list_to_queue([], Queue, Queue).
add_list_to_queue([H|T], Queue, New_queue) :-
    add_to_queue(H, Queue, Temp_queue),
    add_list_to_queue(T, Temp_queue, New_queue).



    %%%%%%% Best first search algorithm%%%%%%%%%
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

    %%%%% operations for state records %%%%%%%
    %
    % These predicates define state records as an adt
    % A state is just a [State, Parent, G_value, H_value, F_value] tuple.
    % Note that this predicate is both a generator and
    % a destructor of records, depending on what is bound
    % precedes is required by the priority queue algorithms

state_record(State, Parent, G, H, F, [State, Parent, G, H, F]).
precedes([_,_,_,_,F1], [_,_,_,_,F2]) :- F1 =< F2.   

    % go initializes Open and CLosed and calls path 
go(Start, Goal,Heur) :- 
    empty_set(Closed),
    empty_sort_queue(Empty_open),
    (Heur == vamp-wolf -> heuristic_vw(Start, Goal, H); heuristic_st(Goal,Start,H)),
    state_record(Start, nil, 0, H, H, First_record),
    insert_sort_queue(First_record, Empty_open, Open),
    path(Open,Closed, Goal, Heur).

    % Path performs a best first search,
    % maintaining Open as a priority queue, and Closed as
    % a set.
    
    % Open is empty; no solution found
path(Open,_,_,_) :- 
    empty_sort_queue(Open),
    write("graph searched, no solution found").

    % The next record is a goal
    % Print out the list of visited states
path(Open, Closed, Goal,_) :- 
    remove_sort_queue(First_record, Open, _),
    state_record(State, _, _, _, _, First_record),
    State = Goal,
    write('Visited:'),write(State),nl,
    write('Solution path is: '), nl,
    printsolution(First_record, Closed).
    
    % The next record is not equal to the goal
    % Generate its children, add to open and continue
    % Note that bagof in AAIS prolog fails if its goal fails, 
    % I needed to use the or to make it return an empty list in this case
path(Open, Closed, Goal,Heur) :- 
    remove_sort_queue(First_record, Open, Rest_of_open),
    head(First_record,Head),write('Visited:'), write(Head),nl,
    (bagof(Child, moves(First_record, Open, Closed, Child, Goal,Heur), Children);Children = []),
    insert_list(Children, Rest_of_open, New_open),
    add_to_set(First_record, Closed, New_closed),
    path(New_open, New_closed, Goal,Heur),!.
    
    % moves generates all children of a state that are not already on
    % open or closed.  The only wierd thing here is the construction
    % of a state record, test, that has unbound variables in all positions
    % except the state.  It is used to see if the next state matches
    % something already on open or closed, irrespective of that states parent
    % or other attributes
    % Also, Ive commented out unsafe since the way I've coded the water jugs 
    % problem I don't really need it.
moves(State_record, Open, Closed,Child, Goal,Heur) :-
    state_record(State, _, G, _,_, State_record),
    move(State, Next),
    % not(unsafe(Next)),
    isSafe(Next),
    state_record(Next, _, _, _, _, Test),
    not(member_sort_queue(Test, Open)),
    not(member_set(Test, Closed)),
    G_new is G + 1,
    (Heur == vamp-wolf -> heuristic_vw(Next, Goal, H);heuristic_st(Goal,Next,H)),
    F is G_new + H,
    state_record(Next, State, G_new, H, F, Child).
    
    %insert_list inserts a list of states obtained from a  call to
    % bagof and  inserts them in a priotrity queue, one at a time
insert_list([], L, L).
insert_list([State | Tail], L, New_L) :-
    insert_sort_queue(State, L, L2),
    insert_list(Tail, L2, New_L).

    % Printsolution prints out the solution path by tracing
    % back through the states on closed using parent links.
printsolution(Next_record, _):-  
    state_record(State, nil, _, _,_, Next_record),
    write(State), nl.
printsolution(Next_record, Closed) :-
    state_record(State, Parent, _, _,_, Next_record),
    state_record(Parent, _, _, _, _, Parent_record),
    member_set(Parent_record, Closed),
    printsolution(Parent_record, Closed),
    write(State), nl.
    

%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

%%%%%%%%%%%%%%%%%%%% stack operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, list based implementation of stacks

    % empty stack generates/tests an empty stack

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

empty_stack([]).

    % member_stack tests if an element is a member of a stack

member_stack(E, S) :- member(E, S).

    % stack performs the push, pop and peek operations
    % to push an element onto the stack
        % ?- stack(a, [b,c,d], S).
    %    S = [a,b,c,d]
    % To pop an element from the stack
    % ?- stack(Top, Rest, [a,b,c]).
    %    Top = a, Rest = [b,c]
    % To peek at the top element on the stack
    % ?- stack(Top, _, [a,b,c]).
    %    Top = a 

stack(E, S, [E|S]).

%%%%%%%%%%%%%%%%%%%% queue operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, list based implementation of 
    % FIFO queues

    % empty queue generates/tests an empty queue


empty_queue([]).

    % member_queue tests if an element is a member of a queue

member_queue(E, S) :- member(E, S).

    % add_to_queue adds a new element to the back of the queue

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

    % remove_from_queue removes the next element from the queue
    % Note that it can also be used to examine that element 
    % without removing it
    
remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :- 
    append(First, Second, Concatenation).

%%%%%%%%%%%%%%%%%%%% set operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, 
    % list based implementation of sets
    
    % empty_set tests/generates an empty set.

empty_set([]).

member_set(E, S) :- member(E, S).

    % add_to_set adds a new member to a set, allowing each element
    % to appear only once

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T) :- !.
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new), !.
    
union([], S, S).
union([H|T], S, S_new) :- 
    union(T, S, S2),
    add_to_set(H, S2, S_new).   
    
intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
    member_set(H, S),
    intersection(T, S, S_new),!.
intersection([_|T], S, S_new) :-
    intersection(T, S, S_new),!.
    
set_diff([], _, []).
set_diff([H|T], S, T_new) :- 
    member_set(H, S), 
    set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :- 
    set_diff(T, S, T_new), !.

subset([], _).
subset([H|T], S) :- 
    member_set(H, S), 
    subset(T, S).

equal_set(S1, S2) :- 
    subset(S1, S2), subset(S2, S1).
    
%%%%%%%%%%%%%%%%%%%%%%% priority queue operations %%%%%%%%%%%%%%%%%%%

    % These predicates provide a simple list based implementation
    % of a priority queue.
    
    % They assume a definition of precedes for the objects being handled
    
empty_sort_queue([]).

member_sort_queue(E, S) :- member(E, S).

insert_sort_queue(State, [], [State]).  
insert_sort_queue(State, [H | T], [State, H | T]) :- 
    precedes(State, H).
insert_sort_queue(State, [H|T], [H | T_new]) :- 
    insert_sort_queue(State, T, T_new). 
    
remove_sort_queue(First, [First|Rest], Rest).

    
%%%%%%%%%%%%%%%%%% Development Tests%%%%%%%%%%%%%%%%%%%%%%%%%%%
%test with solver    
% west to east
testSolver:-solve(state(west,0,0), state(east,3,3),vamp-wolf,hfs).
testSolver1:-solve(state(west,0,0), state(east,3,3),vamp-wolf,bfs).
testSolver2:-solve(state(west,0,0), state(east,3,3),vamp-wolf,dfs).
%east to west     
testSolverR:-solve(state(east,3,3), state(west,0,0),vamp-wolf,hfs).
testSolver1R:-solve(state(east,3,3), state(west,0,0),vamp-wolf,bfs).
testSolver2R:-solve(state(east,3,3), state(west,0,0),vamp-wolf,dfs).    


%log file test cases!!!! 
teststdfs:-solve(state([1,2,0,4,5,6,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,dfs).
teststbfs:-solve(state([1,0,2,4,5,6,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,bfs).
teststhfs:-solve(state([1,6,2,4,5,0,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,hfs).
    
%east to west and west to east w/o solver    
test :- goDfs(state(east,3,3), state(west,0,0)).
test2 :- goDfs(state(west,0,0), state(east,3,3)).
test3 :- goBfs(state(east,3,3), state(west,0,0)).
test4 :- goBfs(state(west,0,0), state(east,3,3)).
test5 :- go(state(east,3,3), state(west,0,0),vamp-wolf).
test6 :- go(state(west,0,0), state(east,3,3),vamp-wolf).
  
%sanity check test cases 
testst1:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]),sliding-tile).
testst2:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,0,12,2,15,6,13,7,11,9,8]),sliding-tile).
testst3:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,15,0,13,7,11,9,8]),sliding-tile).
testst4:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,0,15,13,7,11,9,8]),sliding-tile).
testst5:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,0,2,15,13,7,11,9,8]),sliding-tile).
testst6:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,0,1,6,12,10,2,15,13,7,11,9,8]),sliding-tile).
testst7:-go( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,1,0,6,12,10,2,15,13,7,11,9,8]),sliding-tile).    
testst8:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8])).
testst9:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,0,12,2,15,6,13,7,11,9,8])).
testst10:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,15,0,13,7,11,9,8])).
testst11:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,0,15,13,7,11,9,8])).
testst12:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,0,2,15,13,7,11,9,8])).
testst13:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,0,1,6,12,10,2,15,13,7,11,9,8])).
testst14:-goBfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,1,0,6,12,10,2,15,13,7,11,9,8])).
testst15:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8])).
testst16:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,0,12,2,15,6,13,7,11,9,8])).
testst17:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,15,0,13,7,11,9,8])).
testst18:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,2,0,15,13,7,11,9,8])).
testst19:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,10,1,6,12,0,2,15,13,7,11,9,8])).
testst20:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,0,1,6,12,10,2,15,13,7,11,9,8])).
testst21:-goDfs( state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,3,14,1,0,6,12,10,2,15,13,7,11,9,8])).

    
%testing solver    
testSolver3:-solve(state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]),sliding-tile,hfs).
testSolver4:-solve(state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]),sliding-tile,bfs).
testSolver5:-solve(state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]),sliding-tile,dfs).

