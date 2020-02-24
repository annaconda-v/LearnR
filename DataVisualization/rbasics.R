## Basic R operations ##
########################

### Use R as a simple caclulator
## Adition
4 + 4
## Subtraction
4 - 2
## division
4 / 2
## multiplication
4 * 2
## Exponentiation
2 ^ 5
# Modulo = remainder after division
28 %% 6

### Assignment of values to a variable
#object.name <- value
x <- 4 + 4
class(x)
## x is a 'numeric' object
x <- 'hello'
class(x)
## x is a 'character' object
## NOTE: use <- to assign value to an object. Never use '='. = sign is used to set function parameters

## Calling functions
## function.name(arg1 = value1, arg2 = value2 ...)
seq(1, 10)
x <- seq(1, 10)
class(x)
## Here x is a integer vector

## Defining custom function


