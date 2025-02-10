module Prelude where

class Eq (a :: Type) where
    eq :: a -> a -> Boolean
    notEq :: a -> a -> Boolean

infix 4 eq as ==
infix 4 notEq as /=

class Ord (a :: Type) where
    lessThan :: a -> a -> Boolean
    lessThanOrEq :: a -> a -> Boolean
    greaterThan :: a -> a -> Boolean
    greaterThanOrEq :: a -> a -> Boolean

infix 4 lessThan as <
infix 4 lessThanOrEq as <=
infix 4 greaterThan as >
infix 4 greaterThanOrEq as >=

instance Eq Int where
    eq a b = Builtin.equalsInteger a b
    notEq a b = not (eq a b)

instance Ord Int where
    lessThan a b = Builtin.lessThanInteger a b
    lessThanOrEq a b = Builtin.lessThanEqualsInteger a b
    greaterThan a b = not (lessThanOrEq a b)
    greaterThanOrEq a b = not (lessThan a b)

class Num (a :: Type) where
    add :: a -> a -> a
    subtract :: a -> a -> a
    multiply :: a -> a -> a
    divide :: a -> a -> a

infixr 5 divide as /
infixr 5 multiply as *

infixr 5 add as +
infixr 5 subtract as -

instance Num Int where
    add = Builtin.addInteger
    subtract = Builtin.subtractInteger
    multiply = Builtin.multiplyInteger
    divide = Builtin.divideInteger
