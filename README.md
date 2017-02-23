# descartes: verifico, ergo sum.  

Overview
-------------------------------------------------

descartes is a fully automated cartesian hoare prover
for Java programs.

Installation
===========

Descartes is implemented in Haskell and it uses two 
modified packages that are located in the dependencies
directory: language-java and z3.

To install those packages, just run the command 'cabal install'
in the root directory of the packages (they contain a .cabal file).

To install the main package, run 'cabal install' from the 
descartes directory.

Running Descartes
=================

Once descartes is installed, running 'descartes -h' will 
print the help information.

Currently, descartes receives 2 options '-p' and '-m' to 
specify the hyperproperty to be verified and the mode 
of execution of descartes.

In addition, it also receives a Java file. Note that the
prototype was designed to verify relational operators.

Check the directory 'benchmarks/pldi-16' for examples.

Currently, one can execute descartes to verify the following
hyperproperties related to Java comparators and equals specified 
by the option '-p':

- '-p=1' = (compare): forall x and y, sgn(compare(x,y)) == −sgn(compare(y,x))

- '-p=2' = (compare): for all x, y and z, compare(x, y) > 0 and compare/equals(y, z) > 0 implies compare/equals(x, z) > 0

- '-p=3' = (compare): for all x, y and z, compare(x,y) == 0 implies that sgn(compare(x, z)) == sgn(compare(y, z))

- '-p=4' = (equals): for all x, y and z, equals(x, y) and equals(y, z) implies equals(x, z)

- '-p=5' = (equals): for any non-null reference values x and y, x.equals(y) should return true if and only if y.equals(x) returns true

- '-p=6' = (equals): for any non-null reference values x and y, multiple invocations of x.equals(y) consistently return true or consistently return false

To add new properties, check the files 'src/Analysis/Properties.hs' and the 'src/Main.hs'.

There are 4 modes of execution of 'descartes' that can be specified using the option '-m':

- '-m=0' = No explicit product program construction and with optimisations (havoc)

- '-m=1' = No explicit product program construction without optimisations

- '-m=2' = Verification using self-composition

- '-m=3' = Verification using explicit product construction

Check the paper for more details and the comparison between these modes. 
