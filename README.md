# LambdaLight
LambdaLight is a simple Lambda Calculus interpreter written in Haskell. It includes all of the elements of an evaluation environment, with support for name bindings.

This language was designed to make it easier to teach Lambda Calculus, by being as genuine an analog to the original lambda calculus invented by Alonzo Church, while still being easy to work with in a REPL environment.

## Language
The Language is implemented as a REPL (Read Eval Print Loop).
Unlike LISP, this supports a substitution model, where there is only one global environment, and all functions must be constructed from previously built combinatorial expressions.
The Language supports creating `Var`iables, `Abs`tractions, and `App`lications of anonymous functions:
Typing the following expressions will evaluate the following:
```
λ: -- Lambda Light supports Haskell-like single line comments
<Primitive: None>

λ: x -- Lambda Light supports creating arbitrary variables.
x

λ: \x.x   -- Lambda Light supports the creation lambda expressions using \ and the unicode λ character.
λx.x

λ: (λx.x x) (λx.x)  -- Lambda Light supports function application.
λx.x

λ: true := \x.\y.x  -- Lambda Light supports binding names to expressions.
λx.λy.x

λ: false := \x.\y.y -- Lambda Light supports bindings to a global namespace.
λx.λy.y

λ: not := \b.b false true  -- Lambda Light allows you to create functions with named functions.
λb. b false true

λ: not false
true
```
The last line returns `true` because Lambda Light substitutes a variable for it's relevant binding only when it is being called, instead of when it is an argument to a function. This is called lazy evaluation or normal order evaluation, as opposed to eager or applicative order evaluation.

Once something is defined, it cannot be changed. All definitions in the language are immutible. this helps prevent cheating and defining recursive functions. It also helps impose `referential transparency`.