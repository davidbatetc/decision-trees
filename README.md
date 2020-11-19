# Decision Trees
The following document describes the approach followed to solve the problem presented in [*Decision Trees*](https://gebakx.github.io/hs-dts/), a Haskell programming project of the course *Programming Languages* (Fall semester, 2020), which is part of the course offer of the *Facultat d'Informàtica de Barcelona* (FIB - UPC).


## Short description
We are tasked with creating a Haskell program that allows us to build a **decision tree** from a data set and then classify examples that are *not* part of the data set via user interaction. In particular, the program must be able to process the data set [*Mushroom*](https://archive.ics.uci.edu/ml/datasets/Mushroom), of the *UC Irvine Machine Learning Repository*. A broader description of the problem, along with the requirements of the assignment, can be found [**here**](https://gebakx.github.io/hs-dts/).




## Usage
There are mainly two ways to run this program. The first option is to **compile the program** and then run the compiled file. The second option is to run the program using an **interactive environment**. The [*Glasgow Haskell Compiler (GHC)*](https://www.haskell.org/ghc/) is recommended for both options. The following instructions have been tested in a Linux environment, and they might differ slightly in other operating systems.

### Compiled version
In order to run the compiled program we first have to compile it. This can be achieved by running the following command in a terminal once *ghc* has been installed.
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
In order to run the program in the interactive environment of *ghc* we can use the command
```bash
$ ghci dts.hs
```


## Construction of the decision tree
The decision tree in this program is implemented using as a guideline the description in [*Machine Learning*](https://gebakx.github.io/ml/#35) (pages 35 to 40).

### Definition of the decision tree
Following Haskell's philosophy, the decision tree is defined *in a generalized way*, as follows.

```haskell
data DT a b = Leaf a | Node String [(b, DT a b)]
```

As a consequence, both the classes and the values can be of any given type. Note that this is a recursive definition. A decision tree can either be a **leaf**, or a **node** whose children are decision trees which are accessed via values of an attribute. Hence, it is possible to define, for example,

```haskell
dt = Node "Attribute 1" [(100, Node "Attribute 2" [(55, Leaf 67.54)])]
```

An interesting observation to make is the fact that the base case of the decision tree is not an empty tree, but a tree which only consists of a leaf.


### Feeding data to the decision tree generator
In order to build a decision tree, we need to know which **attributes** we will consider, as well as the **values** of these attributes for different examples. Using the mushrooms as an analogy, the following data type is defined.

```haskell
data Specimen a b = Specimen a [b]
```

The purpose of this definition is to encapsulate the concept of *example to be feeded to the decision tree*, in an attempt to make the code easier to read. Once again, this definition is given in a generalized way. We can then generate a decision tree in the following way.

```haskell
sps = [Specimen "edible" ["convex", "brown"], Specimen "poisonous" ["bell", "brown"]]
attrNames = ["cap-shape", "cap-color"]
dt = generateDT attrNames sps
```

Note that the types of the classes and the values of the attributes of the specimens need to be instances of the `Ord` class for this implementation of the construction of the decision tree. It would be possible, however, to implement it so that they only need to be instances of the `Eq` class.  


### Short note on efficiency
One of the most critical parts of building the decision tree in terms of efficiency is **measuring the accuracy** of each of the attributes in order to decide which is the best one at a given point. In this implementation, this is by done using an algorithm the relies heavily on sorting and grouping repetitions. Since this is the part of the algorithm that has the highest time complexity, it is important to optimize it as much as possible. For this reason, this program includes a **modified merge sort** that sorts a list of elements while counting repetitions. Let us provide a couple of examples:

```haskell
>>>> mgsortBy compare [-1, -1, 2, -1, 4, 5, -1, 5, 4, 2, 5]
[(4,-1),(2,2),(2,4),(3,5)]
>>>> mgsortBy (flip compare) ["brown", "yellow", "brown", "brown", "yellow", "pink"]
[(2,"yellow"),(1,"pink"),(3,"brown")]
```

### Heuristics
During the construction of the decision tree as described in [[4]](https://gebakx.github.io/ml/#35), we might encounter situations in which we cannot immediately decide **which is the best attribute**. What we can do when this happens is use heuristics to choose the attribute which we think will be more convenient.

This program includes **two heuristics** that choose the attribute considered to be the best when the decision procedure described in [[4]](https://gebakx.github.io/ml/#35) results in a draw. If the best attribute cannot be decided, the first heuristic is used. If this is still not enough to decide, then the second heuristic is used. In the case that even the second heuristic fails to decide, the attribute with the greatest index is chosen among those that are tied.

A tie can also happen when finding **which is the most common class** among the specimens that have the same value in a given attribute. For this case, the program does not include an heuristic. Instead, it chooses the greater class among those that are tied, according to the order associated to the type of the classes.


#### Maximizing perfect accuracy
The main objective of the first heuristic is to maximize the number of  specimens for which a prediction of the class can be made already at that point. In this way, it is hoped that the user will need to use less attributes in order to obtain their prediction. This is done by choosing **the attribute which has more specimens for which the specimens with the same value of the attribute are all members of the same class**. In this case, the prediction of the class *in this point* is perfectly accurate (according to the data set). An example is provided below.

```haskell
>>>> attrNames = ["cap-shape", "cap-color"]
>>>> sps = [Specimen "edible" ["convex", "brown"], Specimen "poisonous" ["convex", "brown"], Specimen "edible" ["convex", "white"]]
>>>> generateDT attrNames sps
"cap-color"
  "brown"
    "cap-shape"
      "convex"
        "edible"
  "white"
    "edible"
```


#### Maximizing the number of values
This heuristic consists on choosing **the attribute which has more different values** at that point. The aim of this heuristic is once again to reduce the number of attributes that the user will need to use in order to obtain their prediction. Note that under the assumption that for each attribute, each value has approximately the same amount of specimens, the number of specimens in each of the subtrees arising from the choice of attribute will be less if the chosen attribute has more different values. The hope is that this results in trees which have a lower height. An example in which this heuristic is used is the following.

```haskell
>>>> attrNames = ["cap-shape", "cap-color"]
>>>> sps = [Specimen "edible" ["convex", "brown"], Specimen "edible" ["convex", "white"], Specimen "poisonous" ["bell", "pink"]]
>>>> generateDT attrNames sps
"cap-color"
  "brown"
    "edible"
  "pink"
    "poisonous"
  "white"
    "edible"
```


## Classification
Once compiled and run, what the program does is classify any mushroom based on the decision tree that has been previously built using the file `agaricus-lepiota.data`, extracted from the data set [*Mushroom*](https://archive.ics.uci.edu/ml/datasets/Mushroom). The user is asked to provide the values of different attributes in order to classify the specimen, whose set of values might not match any of the specimens in the data set. The following is an example of execution of the program.

```bash
$ ./dts
```
```
<system> Which odor?
<user> n
<system> Which spore-print-color?
<user> w
<system> Which habitat?
<user> p
<system> Prediction: e
```

A possibility that can happen is that the specimen of the user has a value for a given attribute that does not match any of the decision tree *at that point*. When this happens, an error message is displayed and the program finishes. Internally, this is handled using the `Maybe` and the `Either` types. An example of this is shown below.

```bash
$ ./dts
```

```
<system> Which odor?
<user> n
<system> Which spore-print-color?
<user> x
<system> ERROR. Value "x" for attribute "spore-print-color" missing.
```

## Further comments
### Output coloring
In order to make the program more user-friendly, **ANSI color escape sequences** are used to be able to display different colors when showing the content. While this is a nice feature for those running the program in an environment that supports these escape sequences, it might not look so good if that is not the case. Thus, it is recommended to run the program in an environment that offers support for these.


### Generality of the program
One of the initial intentions when creating this program was **to make it as general as possible**, meaning that the program could be used with different data sets without barely any changes. While this has been achieved for the most part, it has not been entirely possible due to the way in which the type `Char` is read in Haskell in comparison with other types such as `Int` or `Double`.

Take as an example the function `readSpecimen`, which creates a `Specimen` from a `String` whose content is separated by a given `Char`.
```haskell
readSpecimen :: (Read a, Read b) => Char -> String -> Specimen a b
readSpecimen sep str = Specimen (read $ head splitStr) (map read (tail splitStr))
  where splitStr = wordsCustom sep str
```

Let us show the results of running this function with different arguments.
```haskell
>>>> readSpecimen ',' "1,2.34,4.57,3.698" :: Specimen Int Double
Specimen 1 [2.34,4.57,3.698]
>>>> readSpecimen ';' "3.4;(0,1);(1,3);(3,4)" :: Specimen Float (Int, Int)
Specimen 3.4 [(0,1),(1,3),(3,4)]
>>>> readSpecimen ',' "e,c,b,n" :: Specimen Char Char
*** Exception: Prelude.read: no parse
```

Note that in the last case we get an `Exception`. This happens because Haskell's `read` expects `Char`s to be surrounded by `'`. Therefore, if we want to use this function in this case, we have to rewrite the string as
```haskell
>>>> readSpecimen ',' "'e','c','b','n'" :: Specimen Char Char
Specimen 'e' "cbn"
```

Since this is not how the file that contains the data set is formatted (`agaricus-lepiota.data`), a function `readSpecimenCc` is implemented *ad hoc* for this case. Despite the fact that functions such as `readSpecimen` are not used in the main part of the program, they are provided for the sake of generality.


## References
- [[1]](https://gebakx.github.io/hs-dts/) Programming Languages (Fall 2020). [*Decision Trees*](https://gebakx.github.io/hs-dts/), FIB - UPC.
- [[2]](https://archive.ics.uci.edu/ml/datasets/Mushroom) Jeff Schlimmer, 1981. [*Mushroom Data Set*](https://archive.ics.uci.edu/ml/datasets/Mushroom). [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/index.php).
- [[3]](https://www.haskell.org/ghc/) [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/).
- [[4]](https://gebakx.github.io/ml/#35) Gerard Escudero, 2020. [*Machine Learning*](https://gebakx.github.io/ml/#35) (pages 35 to 40)


## To-do list
-   [x] Complete a **first rough version** of the program.
-   [x] Completely **finish specimen classification** with user interaction.
-   [x] Think about **heuristics** for deciding the best attribute for branching when constructing the tree.
-   [x] **Optimize** the construction of the decision tree.
-   [x] Implement the best (subjective) **heuristic** for branching.
-   [x] Implement **safety** with Maybe.
-   [x] Write a proper README.md
-   [ ] Document the code with comments
-   [ ] Delete To-do
