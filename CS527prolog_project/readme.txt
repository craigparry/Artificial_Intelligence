#Prolog Programming -CS527 Programming Assignment 2 
Craig Parry

For you're convinence I have added all of the test cases as predicates 
at the end of the file CS527solver.pl

How to run the program: 
In swipl repl load file with : ['CS527Solver'].

To solve a problem: 

solve(Start,Goal,Problem,Search). 

Arguments- 

(Start & Goal)
Start and Goal are the state representation.
vamp-wolf state = state({west,east},#V,#W). (#V && #W: Max of 3 and Min of 0)
sliding-tile state = state([0,...,15]) (a set in list form, from zero to 15) 

(Problem)
{vamp-wolf,sliding-tile} (input constant representation of problem to solve)


(Search)
{dfs,bfs,hfs} (input constant representation of search strategy)



Examples-
1. All Vampires and Wolves on west side with goal all creatures on east.  

testSolver:-solve(state(west,0,0), state(east,3,3),vamp-wolf,hfs).

2. All Vampires and Wolves on east side with goal all creatures on west.  

testSolverR:-solve(state(east,3,3), state(west,0,0),vamp-wolf,hfs).

3. sliding tile puzzle

testSolver3:-solve(state([5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8]), state([5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]),sliding-tile,hfs).

bugs: for bfs and hfs searches enter a period (.) after the predicate returns true. It hangs there for some reason that I couldn't figure out. 


State Representation:
Vamp-Wolves - 

Each state consists of a the number of vampires and wolves that are on the east side of the Rio Grande,
with a max value of 3 and min value of 0, and the boats position, on the east or west side. 

Sliding-tiles- 

each state is a list containing a set ranging from the values 0 to 15. Since the board is in Row Major 
order, every 4 indices in the list represents a row. 

Moves: 

Vamp-Wolves - The moves we came up with in class were flipping the east/west value and subtracting the number of wolves and vampires, from the V/W variables, that crossed the river when going from east to west and adding the number that corssed to the V/W variables when going from west to east. Using branching logica I was able to reduce the number of rules that I wrote by a half. To cover the entire state valid moves consisted of at least one creature in the boat and at most 2 creatures. They could be mixed or both the same creature.


sliding-tiles- 
Each move was performed by swaping the elements of two indices in the list. The entire state can be represented as moves left, right, up or down as long as they are moves on the board, so indices must be >=0 or <=15. A move left and right consisted of swapping with the +/- 1 index of the current 0 element. A move down or up consisted of swapping with the +/- 4 index of the current 0 element. I used some recursive functions to help me perform the swapping. Using those rules helped me write general swappin moves that reduced the number of move rules to 4.


Potential Extra-Credit (if written well enough):
* measure how long it takes to run BFS, DFS, Best-First search (for each problem). Why do you think the timings are the way they are?


?- time(solve(state(west,0,0), state(east,3,3),vamp-wolf,dfs)).
% 616 inferences, 0.000 CPU in 0.000 seconds (78% CPU, 4775194 Lips)

?- time(solve(state(west,0,0), state(east,3,3),vamp-wolf,bfs)).
% 2,016 inferences, 0.001 CPU in 0.001 seconds (81% CPU, 2608021 Lips)

?- time(solve(state(west,0,0), state(east,3,3),vamp-wolf,hfs)).
% 2,050 inferences, 0.001 CPU in 0.001 seconds (78% CPU, 2949640 Lips)

?- time(solve(state([1,2,0,4,5,6,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,dfs)).
% 1,986 inferences, 0.000 CPU in 0.000 seconds (79% CPU, 8826667 Lips)

time(solve(state([1,0,2,4,5,6,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,bfs)).
% 55,784 inferences, 0.004 CPU in 0.005 seconds (66% CPU, 15431259 Lips)

time(solve(state([1,6,2,4,5,0,3,7,8,9,10,11,12,13,14,15]), state([1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]),sliding-tile,hfs)).
% 33,583 inferences, 0.002 CPU in 0.002 seconds (91% CPU, 18973446 Lips)

The times of the searches is incredibly close for each of these searches because the goal state is close to the start state so that all of the searches are able to approach the goal fairly quickly. But if the solution isn't quite as nice as this example, then the dfs will be extrodinarily slower or may never reach the goal. The reason that BFS and HFS are both slightly slower is beacause of the overhead that they create. DFS just takes the next move available, while BFS has to calculate all moves for that parent state and queue them at each node encountered. HFS has a similar approach, but their search is pruned by the heuristic, so that it starts approaching the goal quickly instead exploring all of each level of the search space. So the main takeaway is that BFS and HFS will incur overhead where DFS wont as much, but that trade off allows our search to approach the goal in optimal and complete manner. 

Sanity Check. Use the following configurations to do a sanity check on your code. Your implementation should be able to find a path for each of these examples when using BFS. Which examples will give you a solution within a reasonable amount of time when using DFS? and when using Best-First Search?

Examples:

1.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,0,14,10,1,3,12,2,15,6,13,7,11,9,8]).
2.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,10,1,0,12,2,15,6,13,7,11,9,8]).
3.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,10,1,6,12,2,15,0,13,7,11,9,8]).
4.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,10,1,6,12,2,0,15,13,7,11,9,8]).
5.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,10,1,6,12,0,2,15,13,7,11,9,8]).
6.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,0,1,6,12,10,2,15,13,7,11,9,8]).
7.go( [5,0,4,14,10,1,3,12,2,15,6,13,7,11,9,8], [5,4,3,14,1,0,6,12,10,2,15,13,7,11,9,8]).


All examples give a solution with a BFS search and Best First Serach Strategy.

Examples 1,2, and 3 gave me a solution in a reasonable amount of time with DFS. The others did not come to a solution. Which I cannot find any direct bug that is correlated to this outcome. I believe that it is most likely taking a long time to explore each branch or potentially running out of memory as it could be overwriting the stack entries making the visited list ineffective. This would cause a circular search that even if given all the time in the world would never complete.
