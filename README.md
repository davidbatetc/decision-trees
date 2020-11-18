# Decision Trees
The following document describes the approach followed to solve the problem presented in [*Decision Trees*](https://gebakx.github.io/hs-dts/), a Haskell programming project of the course *Programming Languages* (Fall semester, 2020), which is part of the course offer of the *Facultat d'Informàtica de Barcelona* (FIB - UPC).


## Short description
We are tasked with creating a Haskell program that allows us to build a **decision tree** from a data set and then classify examples that are *not* part of the data set via user interaction. In particular, the program must be able to process the data set [*Mushroom*](https://archive.ics.uci.edu/ml/datasets/Mushroom), of the *UC Irvine Machine Learning Repository*. A broader description of the problem, along with the requirements of the assignment, can be found [**here**](https://gebakx.github.io/hs-dts/).


## Construction of the decision tree
The decision tree in this program is implemented using as a guideline the description in [Gerard Escudero, 2020, *Machine Learning* (pages 35 to 40)](https://gebakx.github.io/ml/#35).

### Definition of the decision tree
Following the philosophy of Haskell, the decision tree is defined *in a generalized way*, as follows.

```Haskell
data DT a b = Leaf a | Node String [(b, DT a b)]
```

As a consequence, both the classes and the values can be of any given type. Note that this is a recursive definition. A decision tree can either be a **leaf**, or a **node** whose children are decision trees which are accessed via values of an attribute. Hence, it is possible to define, for example,

```Haskell
t = Node "attribute 1" [(100, Leaf 67.54)]
```


## Usage
There are mainly two ways to run this program. The first option is to **compile the program** and then run the compiled file. The second option is to run the program using an **interactive environment**. The [*Glasgow Haskell Compiler (GHC)*](https://www.haskell.org/ghc/) is recommended for both options. The following instructions have been tested in a Linux environment, and they might differ slightly in other operating systems.

### Compiled version
In order to run the program we first have to compile it. This can be achieved by running the following command in a terminal once *ghc* has been installed.
```bash
$ ghc dts.hs
```
It is possible to compile the program using different options, which can be found running
```bash
$ ghc --help
```
Once the program has been compiled it can be run using
```bash
$ ./dts
```

### Interactive environment
In order to run the program in the interactive environment of *ghc* we can use the command.
```bash
$ ghci dts.hs
```


## To-do list
-   [x] Complete a **first rough version** of the program.
-   [x] Completely **finish specimen classification** with user interaction.
-   [x] Think about **heuristics** for deciding the best attribute for branching when constructing the tree.
-   [x] **Optimize** the construction of the decision tree.
-   [x] Implement the best (subjective) **heuristic** for branching.
-   [x] Implement **safety** with Maybe.
-   [ ] Write a proper README.md
-   [ ] Document the code with comments
-   [ ] Delete To-do
