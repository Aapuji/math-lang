# math-lang
A programming language with a built-in and customizable computer algebra system and surrounding features to help interact with it.

Name TBD
# Examples
## Creating a Symbolic Node
```
#= Create a new symbolic node to represent the Lambert W function.
   The actual definition is more complex because the Lambert W function has branches. =#
sym LambertW(z: Complex) := let w = LambertW(z) in
  w * e^w == z;

# Create a "regular" symbolic function to output the symbolic node
let W(z: Complex) = LambertW(z);

# We can add a rewrite rule
rewrite W($x) + W($y) =>
  W(x * y * (1/W(x) + 1/W(y));
```
## Doing Probability
```
enum Food {
  Apple,
  Banana,
  Croissant
}

# I don't fully know how I will tell the compiler that this is a valid probability distribution
let FoodDist(food: Food) = match food {
  Food.Apple     => 0.5,
  Food.Banana    => 0.3,
  Food.Croissant => 0.2
};

using std.Probability.*;

let X ~ Normal(0, 1);
let F ~ FoodDist;

let Y = F == Food.Apple and X < 2;
let Z = F != Food.Croissant;
let p = prob(Y | Z);

print("P(Y | Z) = " + p);
```
## Working with Functions
```
let f(x) = x^2 - 5x + 6;
let x :=
  f(x) = 0;

let g = deriv(f, x);
let y :=
  g(x) = 0;
```
## Systems of Equations
```
let x,y :=
  x + y == 5 and
  x - y == 3;

# x = 4, y = 1
```
## Special Characters and Aliases
```
using std.Constants;

# Aliasing an expression
let \phi = Constants.__GOLDEN_RATIO;
alias φ for \phi;

print(φ);
print(\phi); # the same

# Aliasing an identifier
  # Prime Counting Function
let \pi = _prime_counting_function_defined_elsewhere;
alias π for \pi;

let f(n: Nat) = π(n + 1);
let f(n: Nat) = \pi(n + 1); # the same

# Aliasing an operator
alias ++ for +;
print(1 ++ 2) # 3

# Alias a function as an operator
let f(x) = x + 1;
alias ++ for `1:f`; # only prefix unary operator allowed to be created

let g(x, y) = x * y + 1;
alias ** for `2:g`;
```
