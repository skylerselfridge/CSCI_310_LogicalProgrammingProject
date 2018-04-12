% CSCI 310 Organization of Programming Languages, Spring 2018
% Program #2: Logical Programming / SWI Prolog
% Author: Skyler Selfridge


% Function: "hlbackwards"
% Take list as input and return reversed list

hlbackwards([X|Y],Z,W) :- hlbackwards(Y,[X|Z],W).
hlbackwards([],X,X).
hlbackwards(A,R) :- hlbackwards(A,[],R).

% Function: "llbackwards"
% Take list as input and return reversed list (any sublists also reversed)

llbackwards(L,R) :- rev(L,[],R).

rev([],A,A).
rev([H|T],A,R) :-
    ( is_list(H) ->        % If H is a list
      rev(H,[],X),         %   then reverse H as well
      rev(T,[X|A],R)
    ;
      rev(T,[H|A],R)
    ).

	
% Function: "palindrome"
% Take a list as input and return the same list if it is a palindrome
% otherwise append the reverse of input list to create a new palindrome.

palindrome([],[]).
palindrome([X],[X]).
palindrome(L,L):-
    llbackwards(L,L).

palindrome(X,L):-
    llbackwards(X,[_|T]),
    myappend(X,T,L).
	
% myappend(X,Y,Z)
% X is the first list
% Y is the second list
% Z is the two list appended 
% together

myappend([],Y,Y).
% myappend(X,[],X).
myappend([X|Y],Z,[X|W]) :- myappend(Y,Z,W).


% Function: "permutations"
% Take a list as input and generate all possible permuations
% ---- Having some difficulty returning a single list containing all permutaions 

permutations([],[]).
permutations(List,[H|Perm]):-
    delete(H,List,Rest),
    permutations(Rest,Perm).
delete(X,[X|T],T).
delete(X,[H|T],[H|NT])
    :-delete(X,T,NT).
	
	
% Function "sequence"
% Take a Single number input and return a list as defined by the given sequence. 
	
seq(N,Max,Z,X) :-
	N =< Max,
	N =:= 1 ->
	N2 = 0,
	P = N + 1,
	myappend([N2],Z,Q),
	seq(P,Max,Q,X);
	N =< Max,
	N =:= 2 ->
	N2 = 1,
	P = N + 1,
	myappend([N2],Z,Q),
	seq(P,Max,Q,X);
	N =< Max,
	N >= 3 ->
	% add last two elements of previous list
	first2(Z,Num1,Num2),
	N2 is (2 * Num1) + Num2,
	myappend([N2],Z,Q),
	P = N + 1,
	seq(P,Max,Q,X);
	true,
	hlbackwards(Z,X).
	
	
sequence(N,X):-

	seq(1,N,[], X).


first2([First, Second| Tail],First,Second).

removehead([_|Tail], Tail).


% Function "ionah"
% Take a single number input and describe the towers of hanoi problem solution.

ionah(N):-hionah(N,1,3,2).

hionah(1,X,Y,_):-write('Move disk from peg '),write(X),write(' to peg '),write(Y),nl.
hionah(N,X,Y,Z):-N>1,M is N-1,
  hionah(M,X,Z,Y),
  hionah(1,X,Y,_),
  hionah(M,Z,Y,X).

  
 % Function "argue"
 % Takes a list of english words (Generally in the form of a statement) and returns the statement negated.
 % ---- More words could be added to the definition

argue(L,Result) :-

	rep(L,[],Result).
	
	
rep(L,Z,Result) :-


	splitter(L,H,T),
	H='i', Q = ['you'|Z], rep(T,Q,Result);
	
	splitter(L,H,T),
	H='you', Q = ['i'|Z], rep(T,Q,Result);
	
		splitter(L,H,T),
	H='are', Q = ['not','am'|Z], rep(T,Q,Result);
	
		splitter(L,H,T),
	H='am', Q = ['not','are'|Z], rep(T,Q,Result);

	
	splitter(L,H,T),
	Q = [H|Z], rep(T,Q,Result);
	
	true,
	hlbackwards(Z,Result).
	
splitter([H|T],H,T).



% Function "bubblesort"
% input a list of numbers and returns the list in a sorted ascending order

bubblesort(L,SortedL):-
    swap(L,L1),!,
    bubblesort(L1,SortedL) .

bubblesort(L,L).


swap([X,Y|Z],[Y,X|Z]):-
    X>Y,!.

swap([X|Y],[X|Z]):-
    swap(Y,Z).
