# mu-kanren

#### Overview

This application is a step-by-step evaluator for a dialect of
[microKanren](https://github.com/jasonhemann/microKanren).

Programs define *goals*, and execution finds solutions to those goals if
solutions exist. Run the program by clicking the **Run** button at the
top of the page.

The execution path is chosen by clicking on *subgoals* of the current
goal. Possible paths are highlighted, and can be clicked to continue
execution.

#### Terms

The language consists of two types of terms:

-   Symbols, such as `a`, `b`, `c`, and `nil`.
-   Pairs, such as `(a b)`.

Pairs can be nested to generate more interesting terms, such as
`(a (b (c nil)))`.

During evaluation, terms may also contain *unknowns*, indicated by a
hash symbol: `#1`, `#2`, etc.

#### Goals

There are three basic types of goals - equality, conjunctions and
disjunctions:

##### Equality

An equality goal asserts that two terms should be equal. Here is an
example:

    (= x (y z))

This goal asserts that the term `x` is equal to the pair `(y z)`.

If the current goal is an equality, then it can be clicked, and this
will expand the current substitution.

##### Conjunctions

A conjunction asserts that two or more goals are satisfied
simultaneously. A conjunction is introduced using the `conj` keyword.
Here is an example:

    (conj (= x a)
          (= y b)
    )

This goal asserts that `x` equals `a`, and also that `y` equals `b`.

If the current goal is a conjunction, any of the subgoals can be
clicked. Execution will proceed for the selected subgoal and the
remaining subgoals will be added to the list of *remaining goals*. When
execution of the current goal completes, the first remaining goal will
be executed.

##### Disjunctions

A disjunction asserts that at least one of a set of goals is satisfied.
A disjunction is introduced using the `disj` keyword. Here is an
example:

    (disj (= x a)
          (= x b)
          (= x c)
    )

This goal asserts that `x` is either equals to `a`, `b` or `c`.

If the current goal is a disjunction, any of the subgoals can be
clicked. Execution will proceed only for the selected subgoal.

#### Fresh Names

To generate fresh names, use the `fresh` keyword with one or more names,
and a goal as the last argument:

    (fresh x y z (= x (y z)))

This goal generates three fresh names, and asserts that `x` is equal to
the pair `(y z)`. Execution will try to find terms `x`, `y` and `z` such
that this relation holds.

#### Defining Functions

The *defines* pane can be used to define functions which can be used in
the goal. A function is defined using the `define` keyword, which takes
a list of arguments and the defined goal. For example, to define the
`addo` relation on natural numbers:

    (define addo x y z
      (disj (conj (= x zero)
                  (= y z)
            )
            (fresh p r
              (conj (= x (succ p))
                    (addo p y r)
                    (= z (succ r))
              )
            )
      )
    )

This function can then be used in the goal, to search for numbers which
sum to `(succ (succ zero))` (2):

    (fresh x y
      (addo x y (succ (succ zero)))
    )

Note that functions can be defined recursively.
