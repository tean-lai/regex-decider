Hi! Welcome to my regex decision engine. 

Essentially, this program is an implementation of two regular expression 
decision procedures: one by automata construction and one by brzowski derivatives.

The structure of the programs are as below:

Ast.ml contains the type for regular expressions, as well as functions that 
flatten regular expressions, so we don't have something like Sum [Sum _; Sum _].
We also have a functions that normalize expressions, which comes in useful for 
ensuring termination from recursively running Brzowski derivatives. 

Automata.ml contains the types for DFAs, nfas with epsilon closures, and NFAs 
without. Here contains most of the functions related to converting expressions 
to automaton, as well as converting automatons from NFA to DFA. I use Thompson's 
construction algorithm to convert from expressions to NFAs withe epsilon closures
and powerset construction to go from that to DFAs. 

Derivative.ml contains functions that take Brzowski derivatives of expressions. 
There is a lot of commented code, where I attempted to linearize regexes in 
order to guarentee termination from repeated derivatives. This was before I 
introduced a function to normalize expressions. Getting Brzowski derivatives to 
terminate turned out to be quite tricky.

Parse.ml contains functions that allow conversion from strings to regexes. 
For example, using [parse_exp "a^ * b^ + c"] will output 
Sum [Prod [Star (Char 'a'); Star (Char 'b')]; Char 'c']. It might be a little
confusing, but here, "*" represents multiplication or concatenation while 
"^" represents the Kleene star operation. Parse.ml depends on menhir being 
installed to do the parsing.

Unionfind.ml contains definitions of the unionfind data structure and functions
associated with it. 

Equivalence.ml contains the main decision functions. It relies in the near-linear 
time bisimulation algorithm by Hopcroft and Karp, and it uses the unionfind data.
There are two decision functions here, [decide_w_automaton] and [decide_w_brzowski]. 
These two functions both take in two strings, and will output true or false if 
those strings correspond to eqiuvalent regular languages.


To see a quick demo, you can run [dune exec bin/main.exe], which will show a 
couple examples of the decision algorithms being used. [dune test] will also 
show a couple more examples in the form of ounit tests.

To run your own examples, you could do so in utop. To get to the using 
the decision functions, in the root of the directory, input the following 
commands into terminal:
"dune utop"
"open Regex.Equivalence"





