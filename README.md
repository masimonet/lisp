# My first steps into languages

These are two lisp interpreters, one being functional another one not, (being pedantic the first one would be only functional oriented as it stil allows for defining procedural loops). I think this is language is judged unfairly, apart from creating a number of the building blocks many futute languages, its closures really make it extremely versatile. Just by using macros you could emulatae multiple paradigms of programming. Unfortunately because there are already langauges that fit better the different domains of computer science (python for extreme abstraction, c/c++ for performance critical systems...) these will also be my last steps in this language).


### Debian and Ubuntu:

To use this, it is required to have the common lisp interpreter installed:

```
sudo apt install sbcl
```


To run the tests for both interpreters:

```
sudo bash ./run_tests.sh
```

To run the interpreters:

```
sbcl --script interpreter.lisp
```
```
sbcl --script functional_interpreter.lisp
```

This guide assumes you have already cloned the repo
