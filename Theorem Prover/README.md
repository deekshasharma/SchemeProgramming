This is an automated theorem prover. When provided with a series of facts, your
program should be able to determine whether an additional fact can be validly inferred.

**Basic Version**

Implementation of a statement resolution algorithm. It accepts statements in an abbreviated form of conjunctive
normal form (CNF), where each argument is a list of elements that are implicitly or’d together (a disjunction of
variables). The function return one of the following responses:

1. If the two statements can resolve, it should return the resolution. For example:
```
guile> (resolve ‘(a (NOT b) c) ‘(d (NOT f) b))
(a c d (NOT f))
guile> (resolve ‘(a b) ‘((NOT b)))
(a)
```

2. If the two statements cannot be resolved, it should return False. For example:

```
guile> (resolve ‘(a b) ‘(c d))
#f
guile> (resolve ‘(a b) ‘((NOT a) (NOT b)))
#f
```

3. If the two statements resolve to a contradiction, it should return Contradiction. For example:

```
guile> (resolve ‘(a) ‘((NOT a)))
CONTRADICTION
```

**Intermediate Version**

Implementation of a propositional logic solver that accepts statements in CNF and then performs resolution to answer
propositional logic questions.The solver implements a tell and ask interface on top of
the resolver from the basic assignment, using the same syntax for disjunctive statements. The ask function
returns #t for a statement that can be inferred, or UNKNOWN otherwise. For example:

```
guile> (tell ‘((NOT a) b))
OK
guile> (tell ‘((NOT b) c))
OK
guile> (tell ‘(a))
OK
guile> (ask ‘(a))
#t
guile> (ask ‘(c))
#t
guile> (ask ‘(d))
UNKNOWN
````

**Advanced Version**

Implementation of a propositional logic solver that accepts arbitrary propositional statements, converts them into CNF
and then performs resolution to answer propositional logic questions. In this implementation, the operators need
only support binary and unary operations, as indicated below (i.e., you don’t have to support (OR a b c), but
you do have to support (OR (OR a b) c) and (OR a (OR b c))).

The following operators are supported:
1. (Unary) negation: (NOT a)
2. (Binary) disjunction: (OR a b)
3. (Binary) conjunction: (AND a b)
4. (Binary) implication: (IMPLIES a b)
5. (Binary) biconditional: (BICONDITIONAL a b)


The prover return #t if resolution with the negation of the premise results in a contradiction or
UNKNOWN otherwise. Here’s an example of the expected behavior:

```
guile> (tell ‘(IMPLIES hasDog hasMess))
OK
guile> (tell ‘(IMPLIES hasMess (NOT clean)))
OK
guile> (tell ‘hasDog)
OK
guile> (ask ‘hasDog)
#t
guile> (ask ‘clean)
UNKNOWN
guile> (ask ‘(NOT clean))
#t
guile> (ask ‘(IMPLIES hasDog (NOT clean)))
#t
guile> (ask ‘hasCat)
UNKNOWN
guile> (tell ‘(IMPLIES hasMess hasDog))
OK
guile> (ask ‘(BICONDITIONAL hasMess hasDog))
#t
````
