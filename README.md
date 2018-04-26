# LambdaLight
A simple Lambda Calculus Interpreter written in Haskell. Includes all of the elements of an evaluation environment, with support for name bindings.

This language was designed to make it easier to teach Lambda Calculus, by being as genuine an analog to the lambda calculus.

## Starting off
The interpreter supports creating Variables, Abstractions, and Applications of anonymous functions:

Typing the following epxressions will evaluate the following:
```
λ: -- Lambda Light supports Haskell-like single line comments
<Primitive: None>

λ: x -- Lambda Light supports creating arbitrary variables.
x

λ: \x.x   -- Lambda Light supports the creation lambda expressions using \ and the unicode λ.
λx.x

λ: (λx.x x) (λx.x)  -- Lambda Light supports function application.
λx.x

λ: true := \x.\y.x  -- Lambda Light supports binding names to expressions.
λx.λy.x

λ: false := \x.\y.y -- Lambda Light will add bindings to a global namespace.
λx.λy.y

λ: not := \b.b false true  -- Lambda Light allows you to create functions with named functions.
λb.((b false) true)

λ: not false
true
```
The last line returns `true` because Lambda Light substitutes a variable for it's relevant binding only when it is being called, instead of when it is an argument to a function. This is called lazy evaluation or normal order evaluation, as opposed to eager or applicative order evaluation. There are no closures in this language, no mutable state (since you can't set variables), and no using variables before they are defined (Although technically this is possible because of the global environment table, this is a bug).

