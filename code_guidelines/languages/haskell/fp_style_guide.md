# Haskell Functional Programming Style Guide

**Purpose**: Comprehensive Haskell functional programming guide for Cursor AI  
**Version**: 1.0.0  
**Last Updated**: 2025-11-01  
**Status**: Active - Production Ready  
**Scope**: Haskell projects using GHC 9.6+ and Stack  

---

## Quick Links

**Essential Reading**:
- [CURSOR.md](CURSOR.md) - Mandatory universal rules
- [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md) - Full Foldable/Traversable coverage (Haskell as reference!)
- [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md) - Quick reference

**Related Guides**:
- [python-fp-style-guide.md](python-fp-style-guide.md) - Python FP
- [typescript-fp-style-guide.md](typescript-fp-style-guide.md) - TypeScript FP
- [kotlin-fp-style-guide.md](kotlin-fp-style-guide.md) - Kotlin FP
- [swift-fp-style-guide.md](swift-fp-style-guide.md) - Swift FP
- [rust-fp-style-guide.md](rust-fp-style-guide.md) - Rust FP

---

## Table of Contents

1. [Why Haskell: The Reference Implementation](#1-why-haskell-the-reference-implementation)
2. [Core FP Principles](#2-core-fp-principles)
3. [Type System](#3-type-system)
4. [Typeclasses](#4-typeclasses)
5. [Monad Transformers](#5-monad-transformers)
6. [Lazy Evaluation](#6-lazy-evaluation)
7. [Error Handling](#7-error-handling)
8. [Pattern Matching and ADTs](#8-pattern-matching-and-adts)
9. [Common Libraries](#9-common-libraries)
10. [Testing Patterns](#10-testing-patterns)
11. [Build Tools (Stack)](#11-build-tools-stack)
12. [GHC Extensions](#12-ghc-extensions)
13. [Real-World Example: REST API](#13-real-world-example-rest-api)
14. [Real-World Example: Parser](#14-real-world-example-parser)
15. [Data Structure Patterns](#15-data-structure-patterns)
16. [Mandatory Rules Reference](#16-mandatory-rules-reference)

---

## 1. Why Haskell: The Reference Implementation

### 1.1 Haskell's Unique Position

**Haskell is where functional programming concepts originated**. When we talk about Foldable, Traversable, Monads, and other FP patterns in Python, TypeScript, Rust, Kotlin, or Swift, we're talking about implementations of Haskell concepts.

**Key Positioning**:
- ⭐⭐⭐⭐⭐ **Reference Implementation** - The original definitions
- ⭐⭐⭐⭐⭐ **Pure Functional** - Purity enforced by the type system
- ⭐⭐⭐⭐⭐ **Native HKT** - Full Higher-Kinded Types (no encoding!)
- ⭐⭐⭐⭐⭐ **Lazy by Default** - Call-by-need evaluation
- ⭐⭐⭐⭐⭐ **Type Inference** - Strong types, minimal annotations
- ⭐⭐⭐⭐⭐ **30+ Years Mature** - Battle-tested ecosystem

### 1.2 What Makes Haskell Special

#### Pure Functional Programming
```haskell
-- HASKELL: Pure functions ONLY (except in IO)
add :: Int -> Int -> Int
add x y = x + y

-- Side effects must be explicitly tracked in types:
writeFile :: FilePath -> String -> IO ()

-- Other languages: Side effects can happen anywhere (not type-tracked)
```

#### Native Higher-Kinded Types
```haskell
-- HASKELL: Natural syntax for HKT
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Works for ANY type constructor
instance Functor Maybe where ...
instance Functor [] where ...
instance Functor IO where ...

-- Other languages:
-- - Rust: No HKT (uses associated types)
-- - TypeScript: No HKT (uses interface encoding)
-- - Kotlin: Kind<F, A> encoding (verbose)
```

#### Lazy Evaluation
```haskell
-- HASKELL: Infinite lists are natural
naturals :: [Int]
naturals = [0..]

take 10 naturals  -- [0,1,2,3,4,5,6,7,8,9]

-- This doesn't hang! Lazy evaluation only computes what's needed
take 5 (map (*2) [1..])  -- [2,4,6,8,10]

-- Other languages: Eager evaluation (would hang or run out of memory)
```

#### Type Inference
```haskell
-- HASKELL: Strong types with minimal annotations
compose f g x = f (g x)  -- Type inferred: (b -> c) -> (a -> b) -> a -> c

-- Other languages: Usually need more explicit types
```

### 1.3 When to Use Haskell

**Excellent For** ⭐⭐⭐⭐⭐:
- **Compilers & Interpreters** - Type-safe, compositional
- **Financial Systems** - Correctness, no hidden side effects
- **Web APIs** - Type-safe (Servant)
- **Data Processing Pipelines** - Lazy evaluation, composition
- **Domain-Specific Languages (DSLs)** - Parser combinators, EDSLs
- **Academic/Research** - Cutting-edge FP concepts

**Good For** ⭐⭐⭐⭐:
- Backend services
- Command-line tools
- Concurrent systems
- Mathematical software

**Not Ideal For**:
- Systems programming (use Rust)
- Mobile apps (use Swift/Kotlin)
- Real-time systems with strict latency (GC pauses)
- Heavy GUI applications

### 1.4 Comparison with Other Languages

| Feature | Haskell | Rust | TypeScript | Others |
|---------|---------|------|------------|--------|
| **Purity** | ⭐⭐⭐⭐⭐ Enforced | ⭐⭐⭐ Possible | ⭐⭐⭐ Possible | ⭐⭐⭐ Possible |
| **HKT** | ⭐⭐⭐⭐⭐ Native | ❌ No | ❌ No | ⭐⭐⭐ Encoded |
| **Laziness** | ⭐⭐⭐⭐⭐ Default | ❌ Eager | ❌ Eager | ❌ Eager |
| **Inference** | ⭐⭐⭐⭐⭐ Best | ⭐⭐⭐⭐ Good | ⭐⭐⭐ OK | ⭐⭐⭐ OK |
| **Performance** | ⭐⭐⭐⭐ Good | ⭐⭐⭐⭐⭐ Best | ⭐⭐⭐ OK | ⭐⭐⭐ OK |
| **Ecosystem** | ⭐⭐⭐⭐⭐ Mature | ⭐⭐⭐⭐ Growing | ⭐⭐⭐⭐⭐ Huge | Varies |

**Haskell's Sweet Spot**: Where correctness, maintainability, and expressiveness matter more than raw performance.

---

## 2. Core FP Principles

### 2.1 Purity (Referential Transparency)

**Principle**: Pure functions always return the same output for the same input, with no side effects.

#### Pure Functions
```haskell
-- PURE: No side effects, deterministic
add :: Int -> Int -> Int
add x y = x + y

double :: Int -> Int
double x = x * 2

-- Composition of pure functions is pure
addThenDouble :: Int -> Int -> Int
addThenDouble x y = double (add x y)
```

#### Impure Functions (Must use IO)
```haskell
-- IMPURE: Side effects tracked in type
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
getCurrentTime :: IO UTCTime

-- Can't accidentally mix pure and impure!
-- This won't compile:
-- add :: Int -> Int -> Int
-- add x y = do
--   time <- getCurrentTime  -- ERROR: Can't use IO in pure function
--   return (x + y)
```

#### Benefits of Purity
```haskell
-- 1. Easy to test (no mocking needed)
quickCheck $ \x y -> add x y == add y x  -- Commutative

-- 2. Easy to reason about
result = add 2 3  -- Always 5, no hidden state

-- 3. Safe to memoize
memoizedFib = memoize fib  -- Pure function, safe to cache

-- 4. Parallelizable automatically
parallel :: [a -> b] -> [a] -> [b]  -- Safe because pure!
```

### 2.2 Laziness (Call-by-Need Evaluation)

**Principle**: Expressions are only evaluated when their values are needed.

#### Infinite Data Structures
```haskell
-- Define infinite list
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Take only what you need (lazy evaluation!)
take 10 fibs  -- [0,1,1,2,3,5,8,13,21,34]

-- Infinite list of primes
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

take 10 primes  -- [2,3,5,7,11,13,17,19,23,29]
```

#### Lazy Evaluation Benefits
```haskell
-- 1. Composability: Build complex pipelines without intermediate lists
result = take 10 . filter even . map (*2) $ [1..]
-- No intermediate lists created! All fused by compiler.

-- 2. Short-circuit evaluation
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p = listToMaybe . filter p
-- Stops as soon as first element found

-- 3. Separation of concerns
data Stream a = Cons a (Stream a)

-- Generate stream (infinite)
from n = Cons n (from (n + 1))

-- Consume stream (finite)
takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Cons x xs) = x : takeS (n-1) xs

takeS 5 (from 0)  -- [0,1,2,3,4]
```

#### When to Be Strict
```haskell
-- Use strictness annotations to avoid space leaks
import Data.List (foldl')

-- SPACE LEAK (lazy accumulator):
sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0  -- Builds huge thunk!

-- CORRECT (strict accumulator):
sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0  -- Evaluates immediately

-- Explicit strictness with bang patterns:
{-# LANGUAGE BangPatterns #-}

factorial :: Integer -> Integer
factorial = go 1
  where
    go !acc 0 = acc  -- ! forces strict evaluation
    go !acc n = go (acc * n) (n - 1)
```

### 2.3 Type-Driven Development

**Principle**: Let types guide your implementation.

#### Types as Specifications
```haskell
-- The type tells you what the function does:
map :: (a -> b) -> [a] -> [b]
-- Takes a function and a list, returns transformed list

filter :: (a -> Bool) -> [a] -> [a]
-- Takes a predicate and a list, returns filtered list

-- Type-driven: Write type first, implementation follows
traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse _ [] = pure []
traverse f (x:xs) = (:) <$> f x <*> traverse f xs
-- Type guides the implementation!
```

#### Impossible States Made Unrepresentable
```haskell
-- BAD: Can represent invalid states
data UserBad = UserBad
  { userId :: Maybe Int
  , userName :: Maybe String
  }
-- What if both are Nothing? Invalid!

-- GOOD: Only valid states representable
data User
  = Anonymous
  | LoggedIn Int String  -- ID and name always together
  
-- Can't create invalid user!
```

#### Type Holes
```haskell
-- Use _ as placeholder to get type information
example :: (a -> b) -> Maybe a -> Maybe b
example f ma = _

-- GHC tells you:
-- Found hole '_' with type: Maybe b
-- Relevant bindings include:
--   f :: a -> b
--   ma :: Maybe a
--   example :: (a -> b) -> Maybe a -> Maybe b

-- Type-driven implementation:
example f ma = fmap f ma  -- Types guide you!
```

### 2.4 Immutability

**Principle**: All data structures are immutable by default.

#### Immutable Data
```haskell
-- All values are immutable
x = 10
-- x = 20  -- ERROR: Can't reassign

-- "Update" creates new value
data Person = Person { name :: String, age :: Int }

john = Person "John" 30

-- "Update" age (creates new Person)
older = john { age = 31 }  -- john unchanged!
```

#### Persistent Data Structures
```haskell
import qualified Data.Map as Map

-- Maps are immutable, but efficient (structural sharing)
m1 = Map.fromList [(1, "one"), (2, "two")]
m2 = Map.insert 3 "three" m1  -- m1 unchanged, m2 is new map

-- Both m1 and m2 share most structure (efficient!)
```

#### Benefits
```haskell
-- 1. Thread-safe by default (no locks needed!)
parMap :: (a -> b) -> [a] -> [b]  -- Safe! Immutable data

-- 2. Easy to reason about
let x = [1,2,3]
    y = reverse x
in x ++ y  -- x is still [1,2,3], always!

-- 3. Time travel debugging
type History a = [a]

addHistory :: a -> History a -> History a
addHistory x xs = x : xs  -- Old states preserved!
```

---

## 3. Type System

### 3.1 Type Inference (Hindley-Milner)

**Haskell has the best type inference of all mainstream languages**.

#### Automatic Type Inference
```haskell
-- No type annotations needed (but recommended for top-level)
compose f g x = f (g x)
-- Inferred type: (b -> c) -> (a -> b) -> a -> c

twice f x = f (f x)
-- Inferred type: (a -> a) -> a -> a

-- Type inference works through complex expressions
result = map ((*2) . (+1)) [1,2,3]
-- Inferred type: [Int]
-- map :: (a -> b) -> [a] -> [b]
-- (*2) . (+1) :: Num a => a -> a
```

#### Type Annotations (Best Practice)
```haskell
-- RECOMMENDED: Annotate top-level functions
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Annotations help:
-- 1. Documentation
-- 2. Better error messages
-- 3. Catch mistakes early
-- 4. Guide implementation
```

### 3.2 Higher-Kinded Types (Native!)

**Haskell has native HKT - no encoding needed!**

#### Type Constructors
```haskell
-- * is the kind of concrete types (Int, String, etc.)
-- * -> * is the kind of type constructors (Maybe, [], etc.)
-- * -> * -> * is the kind of binary type constructors (Either, Map, etc.)

-- Examples:
-- Int :: *
-- Maybe :: * -> *
-- Maybe Int :: *
-- Either :: * -> * -> *
-- Either String Int :: *
```

#### Functor: HKT in Action
```haskell
-- Functor works over ANY type constructor (f :: * -> *)
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Works for Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- Works for lists
instance Functor [] where
  fmap = map

-- Works for Either e
instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

-- Works for IO
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)

-- ONE interface, works for ALL type constructors!
```

#### Comparison with Other Languages
```haskell
-- HASKELL: Native HKT (beautiful!)
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- RUST: No HKT (uses associated types instead)
-- trait Iterator {
--     type Item;
--     fn map<B, F>(self, f: F) -> Map<Self, F>
--     where F: FnMut(Self::Item) -> B;
-- }

-- TYPESCRIPT: No HKT (uses interface encoding, verbose)
-- interface Functor<F> {
--   map<A, B>(f: (a: A) => B): (fa: HKT<F, A>) => HKT<F, B>
-- }

-- KOTLIN: Kind<F, A> encoding (Arrow library, still verbose)
-- interface Functor<F> {
--   fun <A, B> Kind<F, A>.map(f: (A) -> B): Kind<F, B>
-- }
```

### 3.3 Type Families

**Type-level functions** for advanced type manipulation.

#### Type Families Example
```haskell
{-# LANGUAGE TypeFamilies #-}

-- Type family: associates types with types
type family Element c where
  Element [a] = a
  Element (Map k v) = (k, v)
  Element Text = Char

-- Use in function
headOf :: Element c ~ a => c -> Maybe a
```

#### Associated Types
```haskell
class Collection c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool

instance Collection [a] where
  type Elem [a] = a
  empty = []
  insert = (:)
  member = elem
```

### 3.4 GADTs (Generalized Algebraic Data Types)

**More expressive data types** with refined type information.

#### GADT Example
```haskell
{-# LANGUAGE GADTs #-}

-- Regular ADT (less precise types)
data ExprRegular
  = LitR Int
  | AddR ExprRegular ExprRegular
  | IsZeroR ExprRegular  -- Type mismatch possible!

-- GADT (precise types!)
data Expr a where
  Lit :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

-- Type-safe evaluation (no runtime errors!)
eval :: Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (IsZero e) = eval e == 0
eval (If cond t f) = if eval cond then eval t else eval f

-- Usage
example :: Expr Int
example = If (IsZero (Lit 0)) (Lit 1) (Lit 2)

eval example  -- 1 (type-safe!)
```

### 3.5 Phantom Types

**Types used only for compile-time safety** (not runtime).

#### Phantom Type Example
```haskell
-- Safe units of measurement
data Meter
data Foot

newtype Distance unit = Distance Double

meters :: Double -> Distance Meter
meters = Distance

feet :: Double -> Distance Foot
feet = Distance

-- Type-safe conversion
feetToMeters :: Distance Foot -> Distance Meter
feetToMeters (Distance f) = Distance (f * 0.3048)

-- Type error! Can't mix units
-- addDistances :: Distance Meter -> Distance Foot -> Distance Meter
-- addDistances (Distance m) (Distance f) = Distance (m + f)  -- ERROR!

-- Must convert first
addDistances :: Distance Meter -> Distance Foot -> Distance Meter
addDistances d1 d2 = d1 `addMeters` feetToMeters d2
  where
    addMeters (Distance m1) (Distance m2) = Distance (m1 + m2)
```

---

## 4. Typeclasses

**Haskell invented typeclasses** - here are the original definitions!

### 4.1 Functor

**The original Functor typeclass:**

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g
```

#### Common Functors
```haskell
-- Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- List
instance Functor [] where
  fmap = map

-- Either e
instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

-- Functions ((->) r)
instance Functor ((->) r) where
  fmap = (.)  -- Function composition!
```

#### Using Functor
```haskell
-- Map over Maybe
fmap (+1) (Just 5)  -- Just 6
fmap (+1) Nothing   -- Nothing

-- Map over list
fmap (*2) [1,2,3]  -- [2,4,6]

-- Map over Either
fmap length (Right "hello")  -- Right 5
fmap length (Left "error")   -- Left "error"

-- Infix operator
(+1) <$> Just 5  -- Just 6
-- <$> is infix fmap
```

### 4.2 Applicative

**Sequential application with effects:**

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Laws:
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
```

#### Common Applicatives
```haskell
-- Maybe
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> mx = fmap f mx

-- List
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

-- Either e
instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> x = fmap f x
```

#### Using Applicative
```haskell
-- Apply function in context
pure (+) <*> Just 3 <*> Just 5  -- Just 8

-- Infix operators
(+) <$> Just 3 <*> Just 5  -- Just 8

-- Multi-argument functions
data User = User String Int

-- All-or-nothing validation
makeUser :: Maybe String -> Maybe Int -> Maybe User
makeUser mName mAge = User <$> mName <*> mAge

makeUser (Just "Alice") (Just 30)  -- Just (User "Alice" 30)
makeUser (Just "Alice") Nothing    -- Nothing

-- Applicative style (beautiful!)
user = User <$> getName <*> getAge
```

### 4.3 Monad

**The famous Monad!** Sequential composition with binding.

```haskell
class Applicative m => Monad m where
  return :: a -> m a  -- Same as pure
  (>>=) :: m a -> (a -> m b) -> m b  -- Bind

-- Laws:
-- return a >>= f = f a
-- m >>= return = m
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

#### Common Monads
```haskell
-- Maybe
instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

-- List
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)

-- Either e
instance Monad (Either e) where
  return = Right
  Left e >>= _ = Left e
  Right x >>= f = f x
```

#### Using Monad
```haskell
-- Maybe: early exit on Nothing
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

compute :: Double -> Double -> Double -> Maybe Double
compute x y z = do
  a <- safeDivide x y     -- Exit if Nothing
  b <- safeDivide a z     -- Exit if Nothing
  return (b * 2)

compute 10 2 5  -- Just 2.0
compute 10 0 5  -- Nothing (early exit!)

-- List: non-determinism
pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

pairs [1,2] [3,4]  -- [(1,3),(1,4),(2,3),(2,4)]
```

#### do-notation (Syntactic Sugar)
```haskell
-- These are equivalent:
-- do-notation (readable)
example1 = do
  x <- Just 3
  y <- Just 5
  return (x + y)

-- Explicit bind (what it desugars to)
example2 = 
  Just 3 >>= \x ->
  Just 5 >>= \y ->
  return (x + y)

-- Both return: Just 8
```

### 4.4 Foldable

**The original Foldable typeclass!**

```haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m
  -- ... more methods

-- This is what other languages approximate!
-- - Rust: Iterator trait (fold, sum)
-- - TypeScript: Array.reduce
-- - Others: reduce/fold functions
```

#### Using Foldable
```haskell
-- Sum elements
sum :: (Foldable t, Num a) => t a -> a
sum = foldl (+) 0

-- Product
product :: (Foldable t, Num a) => t a -> a
product = foldl (*) 1

-- Convert to list
toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- Works on any Foldable!
sum [1,2,3]           -- 6
sum (Just 5)          -- 5
sum Nothing           -- 0
sum (Left "error")    -- 0
sum (Right 10)        -- 10
```

### 4.5 Traversable

**The original Traversable typeclass!**

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)

-- This is the gold standard!
-- Other languages approximate this:
-- - Rust: collect() with Result/Option
-- - TypeScript: fp-ts traverse with HKT encoding
-- - Others: Custom implementations
```

#### Using Traversable
```haskell
-- Validate all elements
validatePositive :: Int -> Maybe Int
validatePositive n
  | n > 0 = Just n
  | otherwise = Nothing

-- Traverse with Maybe (stops at first Nothing)
traverse validatePositive [1,2,3]    -- Just [1,2,3]
traverse validatePositive [1,-2,3]   -- Nothing

-- Traverse with Either
validateE :: Int -> Either String Int
validateE n
  | n > 0 = Right n
  | otherwise = Left $ show n ++ " is not positive"

traverse validateE [1,2,3]    -- Right [1,2,3]
traverse validateE [1,-2,3]   -- Left "-2 is not positive"

-- Traverse with IO
traverse print [1,2,3]  -- Prints 1, 2, 3 and returns [(),(),()]
```

---

## 5. Monad Transformers

**Stack monads** to combine effects.

### 5.1 The Problem

```haskell
-- Want to combine Maybe and IO?
type Result = IO (Maybe a)  -- Nested, awkward!

-- Nested do-notation is ugly:
ugly :: IO (Maybe Int)
ugly = do
  mx <- getMaybeValue
  case mx of
    Nothing -> return Nothing
    Just x -> do
      my <- getAnotherMaybeValue
      case my of
        Nothing -> return Nothing
        Just y -> return (Just (x + y))
```

### 5.2 Monad Transformers

**Add capabilities to monads:**

```haskell
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

-- MaybeT adds Maybe to any monad
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- Clean do-notation!
clean :: MaybeT IO Int
clean = do
  x <- MaybeT getMaybeValue        -- Exits if Nothing
  y <- MaybeT getAnotherMaybeValue -- Exits if Nothing
  return (x + y)

-- Run it
main = do
  result <- runMaybeT clean
  print result  -- Maybe Int
```

### 5.3 Common Transformers

#### ExceptT (Error Handling)
```haskell
import Control.Monad.Trans.Except

type AppM = ExceptT String IO

-- Throw errors
throwError :: String -> AppM a

-- Handle errors
catchError :: AppM a -> (String -> AppM a) -> AppM a

-- Example
processFile :: FilePath -> AppM String
processFile path = do
  exists <- lift $ doesFileExist path
  unless exists $
    throwError $ "File not found: " ++ path
  lift $ readFile path
```

#### ReaderT (Dependency Injection)
```haskell
import Control.Monad.Trans.Reader

data Config = Config
  { dbConnection :: String
  , apiKey :: String
  }

type App = ReaderT Config IO

-- Access config
getConfig :: App Config
getConfig = ask

-- Example
runApp :: Config -> App a -> IO a
runApp = flip runReaderT

fetchData :: App String
fetchData = do
  config <- ask
  lift $ putStrLn $ "Using API key: " ++ apiKey config
  return "data"
```

#### StateT (Stateful Computation)
```haskell
import Control.Monad.Trans.State

type Counter = StateT Int IO

-- Modify state
increment :: Counter ()
increment = modify (+1)

-- Get state
getCount :: Counter Int
getCount = get

-- Example
program :: Counter ()
program = do
  increment
  increment
  count <- getCount
  lift $ print count  -- Prints 2

runStateT program 0  -- ((), 2)
```

### 5.4 Monad Transformer Stack

**Combine multiple transformers:**

```haskell
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

data Config = Config { maxRetries :: Int }
type AppError = String

-- Stack: ReaderT + ExceptT + IO
type App = ReaderT Config (ExceptT AppError IO)

-- Run the stack
runApp :: Config -> App a -> IO (Either AppError a)
runApp config app = runExceptT (runReaderT app config)

-- Use the stack
fetchData :: String -> App String
fetchData url = do
  config <- ask                    -- ReaderT
  retries <- return (maxRetries config)
  when (retries <= 0) $
    throwError "No retries left"   -- ExceptT
  lift $ lift $ putStrLn $ "Fetching " ++ url  -- IO
  return "data"
```

### 5.5 MTL Style

**The `mtl` library** provides type classes for transformer capabilities:

```haskell
import Control.Monad.Reader
import Control.Monad.Except

-- Constraints instead of concrete transformers
type App m = (MonadReader Config m, MonadError AppError m, MonadIO m)

fetchData :: App m => String -> m String
fetchData url = do
  config <- ask              -- MonadReader
  throwError "Error"         -- MonadError
  liftIO $ putStrLn url      -- MonadIO
  return "data"

-- Works with any transformer stack that has these capabilities!
```

---

## 6. Lazy Evaluation

**Haskell is lazy by default** - expressions evaluated only when needed.

### 6.1 Infinite Data Structures

```haskell
-- Define infinite lists naturally
naturals :: [Int]
naturals = [0..]

-- Fibonacci sequence (infinite)
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

take 10 fibs  -- [0,1,1,2,3,5,8,13,21,34]

-- Prime numbers (infinite, using Sieve of Eratosthenes)
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

take 10 primes  -- [2,3,5,7,11,13,17,19,23,29]
```

### 6.2 Lazy Evaluation Benefits

#### Fusion and Deforestation
```haskell
-- This creates NO intermediate lists!
result = take 10 . filter even . map (*2) $ [1..]

-- Compiler fuses operations:
-- 1. map (*2)
-- 2. filter even
-- 3. take 10
-- All happen in one pass!
```

#### Short-Circuit Evaluation
```haskell
-- find: stops at first match
find :: (a -> Bool) -> [a] -> Maybe a
find p = listToMaybe . filter p

find (> 100) [1..]  -- Just 101 (doesn't evaluate entire infinite list!)

-- any: stops at first True
any even [1,3,5,7,1000,1001..]  -- True (stops at 1000)
```

#### Separation of Generation and Consumption
```haskell
-- Generate (infinite)
squares :: [Integer]
squares = [n^2 | n <- [1..]]

-- Consume (finite)
firstTenSquares = take 10 squares  -- [1,4,9,16,25,36,49,64,81,100]

-- Generator doesn't care about consumer!
-- Consumer doesn't care about generator!
```

### 6.3 When Laziness Causes Problems

#### Space Leaks
```haskell
-- BAD: Lazy accumulator causes space leak
sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0  -- Builds huge thunk: (((0 + 1) + 2) + 3)...

-- GOOD: Strict accumulator
import Data.List (foldl')

sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0  -- Evaluates immediately
```

#### Bang Patterns
```haskell
{-# LANGUAGE BangPatterns #-}

-- Force strict evaluation
factorial :: Integer -> Integer
factorial = go 1
  where
    go !acc 0 = acc        -- ! forces evaluation
    go !acc n = go (acc * n) (n - 1)
```

#### Strict Fields
```haskell
-- Lazy fields (default)
data PersonLazy = PersonLazy String Int

-- Strict fields
data PersonStrict = PersonStrict !String !Int
-- ! ensures fields are evaluated immediately
```

### 6.4 DeepSeq for Full Evaluation
```haskell
import Control.DeepSeq

-- Force full evaluation
forceEval :: NFData a => a -> a
forceEval x = x `deepseq` x

-- Example: Evaluate nested structure
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Generic

instance NFData a => NFData (Tree a)

-- Force entire tree evaluation
evaluatedTree = force myTree  -- All nodes evaluated
```

---

## 7. Error Handling

**Haskell uses types for error handling** - no exceptions in pure code!

### 7.1 Maybe Type

**For operations that might fail (no error details):**

```haskell
data Maybe a = Nothing | Just a

-- Safe division
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Safe head
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Safe lookup
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = Nothing
lookup key ((k,v):rest)
  | key == k = Just v
  | otherwise = lookup key rest
```

#### Working with Maybe
```haskell
-- Pattern matching
case safeDivide 10 2 of
  Nothing -> putStrLn "Division by zero"
  Just result -> print result

-- maybe function
maybe 0 (*2) (Just 5)  -- 10
maybe 0 (*2) Nothing   -- 0

-- Functor
fmap (+1) (Just 5)     -- Just 6
fmap (+1) Nothing      -- Nothing

-- Applicative
(+) <$> Just 3 <*> Just 5  -- Just 8
(+) <$> Nothing <*> Just 5 -- Nothing

-- Monad (short-circuit on Nothing)
do
  x <- Just 3
  y <- Nothing
  return (x + y)  -- Nothing (short-circuited!)
```

### 7.2 Either Type

**For operations that might fail (with error details):**

```haskell
data Either a b = Left a | Right b

-- By convention: Left is error, Right is success
validateAge :: Int -> Either String Int
validateAge age
  | age < 0 = Left "Age cannot be negative"
  | age > 150 = Left "Age too high"
  | otherwise = Right age

validateEmail :: String -> Either String String
validateEmail email
  | '@' `elem` email = Right email
  | otherwise = Left "Invalid email"
```

#### Working with Either
```haskell
-- Pattern matching
case validateAge (-5) of
  Left err -> putStrLn $ "Error: " ++ err
  Right age -> print age

-- either function
either putStrLn print (validateAge 30)

-- Functor (maps over Right)
fmap (+1) (Right 5)     -- Right 6
fmap (+1) (Left "err")  -- Left "err"

-- Monad (short-circuit on Left)
do
  age <- validateAge 30
  email <- validateEmail "test@example.com"
  return (age, email)
-- Right (30, "test@example.com")

do
  age <- validateAge (-5)
  email <- validateEmail "test@example.com"
  return (age, email)
-- Left "Age cannot be negative" (short-circuited!)
```

### 7.3 ExceptT Monad Transformer

**Combine Either with IO:**

```haskell
import Control.Monad.Trans.Except

type AppError = String
type App = ExceptT AppError IO

-- Throw error
throwE :: AppError -> App a

-- Example
loadConfig :: FilePath -> App Config
loadConfig path = do
  exists <- lift $ doesFileExist path
  unless exists $
    throwE $ "Config file not found: " ++ path
  content <- lift $ readFile path
  case parseConfig content of
    Nothing -> throwE "Invalid config format"
    Just config -> return config

-- Run
main = do
  result <- runExceptT (loadConfig "config.yaml")
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right config -> print config
```

### 7.4 Error vs Exception

```haskell
-- GOOD: Use types for expected errors
safeDivide :: Double -> Double -> Either String Double
safeDivide _ 0 = Left "Division by zero"
safeDivide x y = Right (x / y)

-- BAD: Don't use exceptions in pure code
-- badDivide :: Double -> Double -> Double
-- badDivide _ 0 = error "Division by zero"  -- DON'T DO THIS!
-- badDivide x y = x / y

-- Exceptions only for truly exceptional cases (IO)
readFileStrict :: FilePath -> IO String
readFileStrict path = do
  handle (\(e :: IOException) -> return "") $
    readFile path  -- IOException is appropriate here
```

### 7.5 Validation (Multiple Errors)

**Collect all errors (not short-circuit):**

```haskell
import Data.Validation

-- Validation accumulates errors (unlike Either)
data Validation e a = Failure e | Success a

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)  -- Combine errors!
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e
  Success f <*> Success x = Success (f x)

-- Validate user (collect ALL errors)
data User = User String String Int

validateUser :: String -> String -> Int -> Validation [String] User
validateUser name email age =
  User <$> validateName name
       <*> validateEmail email
       <*> validateAge age
  where
    validateName n
      | length n >= 2 = Success n
      | otherwise = Failure ["Name too short"]
    
    validateEmail e
      | '@' `elem` e = Success e
      | otherwise = Failure ["Invalid email"]
    
    validateAge a
      | a >= 0 && a <= 150 = Success a
      | otherwise = Failure ["Invalid age"]

-- Get ALL errors at once!
validateUser "A" "bademail" (-5)
-- Failure ["Name too short", "Invalid email", "Invalid age"]
```

---

## 8. Pattern Matching and ADTs

**Algebraic Data Types** are sum types (OR) and product types (AND).

### 8.1 Sum Types (OR)

```haskell
-- Either this OR that
data Bool = True | False

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

-- Traffic light
data TrafficLight = Red | Yellow | Green

-- Pattern match (exhaustive!)
action :: TrafficLight -> String
action Red = "Stop"
action Yellow = "Caution"
action Green = "Go"
-- Compiler warns if not exhaustive!
```

### 8.2 Product Types (AND)

```haskell
-- This AND that
data Point = Point Double Double  -- x AND y

data Person = Person
  { name :: String    -- Name AND
  , age :: Int        -- Age AND
  , email :: String   -- Email
  }

-- Tuple (anonymous product)
type Coord = (Double, Double)
```

### 8.3 Recursive Types

```haskell
-- List (built-in syntax)
data List a = [] | a : (List a)

-- Binary tree
data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

-- Example
exampleTree :: Tree Int
exampleTree = Branch
  (Branch (Leaf 1) (Leaf 2))
  (Leaf 3)

-- Fold over tree
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leaf branch tree =
  case tree of
    Leaf x -> leaf x
    Branch left right ->
      branch (foldTree leaf branch left) (foldTree leaf branch right)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

sumTree exampleTree  -- 6
```

### 8.4 Pattern Matching

```haskell
-- Basic patterns
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- List patterns
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Nested patterns
describe :: Maybe (Either String Int) -> String
describe Nothing = "Nothing"
describe (Just (Left err)) = "Error: " ++ err
describe (Just (Right n)) = "Number: " ++ show n

-- Guards
abs :: Int -> Int
abs n
  | n < 0 = -n
  | otherwise = n

-- where clauses
quadraticRoots :: Double -> Double -> Double -> (Double, Double)
quadraticRoots a b c = (root1, root2)
  where
    discriminant = b^2 - 4*a*c
    root1 = (-b + sqrt discriminant) / (2*a)
    root2 = (-b - sqrt discriminant) / (2*a)

-- case expressions
max :: Ord a => a -> a -> a
max x y = case compare x y of
  GT -> x
  EQ -> x
  LT -> y
```

### 8.5 Smart Constructors

**Enforce invariants at construction:**

```haskell
-- Don't export constructor
module User (User, makeUser, getUserEmail) where

data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  }

-- Smart constructor (only way to create User)
makeUser :: String -> String -> Int -> Either String User
makeUser name email age
  | length name < 2 = Left "Name too short"
  | not ('@' `elem` email) = Left "Invalid email"
  | age < 0 || age > 150 = Left "Invalid age"
  | otherwise = Right (User name email age)

-- Accessor
getUserEmail :: User -> String
getUserEmail = userEmail

-- Now impossible to create invalid User!
```

---

## 9. Common Libraries

### 9.1 base (Standard Library)

**Included with GHC:**

```haskell
-- Prelude (imported by default)
import Prelude

-- Data structures
import Data.List      -- List operations
import Data.Maybe     -- Maybe utilities
import Data.Either    -- Either utilities

-- Control
import Control.Monad  -- Monad utilities
import Control.Applicative

-- System
import System.IO      -- I/O operations
import System.Environment  -- Command-line args
```

#### Common Functions
```haskell
-- List operations
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldr :: (a -> b -> b) -> b -> [a] -> b
zip :: [a] -> [b] -> [(a, b)]
concat :: [[a]] -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]

-- Maybe utilities
maybe :: b -> (a -> b) -> Maybe a -> b
fromMaybe :: a -> Maybe a -> a
listToMaybe :: [a] -> Maybe a
catMaybes :: [Maybe a] -> [a]

-- Either utilities
either :: (a -> c) -> (b -> c) -> Either a b -> c
lefts :: [Either a b] -> [a]
rights :: [Either a b] -> [b]
```

### 9.2 containers

**Efficient data structures:**

```haskell
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- Map (dictionary)
phoneBook :: Map.Map String String
phoneBook = Map.fromList
  [ ("Alice", "555-1234")
  , ("Bob", "555-5678")
  ]

Map.lookup "Alice" phoneBook  -- Just "555-1234"
Map.insert "Charlie" "555-9012" phoneBook

-- Set
numbers :: Set.Set Int
numbers = Set.fromList [1,2,3,2,1]  -- {1,2,3}

Set.member 2 numbers  -- True
Set.insert 4 numbers  -- {1,2,3,4}

-- Sequence (efficient list)
seq :: Seq.Seq Int
seq = Seq.fromList [1,2,3]

Seq.index seq 1  -- 2
seq Seq.|> 4     -- [1,2,3,4] (efficient append!)
```

### 9.3 text

**Efficient Unicode text:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Use Text instead of String!
greeting :: T.Text
greeting = "Hello, 世界!"

-- Text operations
T.length greeting         -- 9
T.toUpper greeting        -- "HELLO, 世界!"
T.splitOn "," greeting    -- ["Hello", " 世界!"]

-- I/O with Text
main :: IO ()
main = do
  content <- TIO.readFile "input.txt"
  TIO.putStrLn content
```

### 9.4 bytestring

**Efficient binary data:**

```haskell
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- Read binary file
readBinary :: FilePath -> IO BS.ByteString
readBinary = BS.readFile

-- Write binary file
writeBinary :: FilePath -> BS.ByteString -> IO ()
writeBinary = BS.writeFile
```

### 9.5 mtl (Monad Transformer Library)

**Monad transformers:**

```haskell
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- See section 5 (Monad Transformers)
```

### 9.6 aeson (JSON)

**JSON parsing and generation:**

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

-- Encode to JSON
encode (User 1 "Alice" "alice@example.com")
-- {"userId":1,"userName":"Alice","userEmail":"alice@example.com"}

-- Decode from JSON
decode "{\"userId\":1,\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}" :: Maybe User
-- Just (User 1 "Alice" "alice@example.com")
```

### 9.7 vector

**Efficient arrays:**

```haskell
import qualified Data.Vector as V

-- Create vector
vec :: V.Vector Int
vec = V.fromList [1,2,3,4,5]

-- Indexing (O(1))
vec V.! 2  -- 3

-- Map
V.map (*2) vec  -- [2,4,6,8,10]

-- Much faster than lists for large data!
```

---

## 10. Testing Patterns

**Haskell has the best testing tools!** QuickCheck originated here.

### 10.1 QuickCheck (Property-Based Testing)

**QuickCheck invented property-based testing!**

#### Basic Properties
```haskell
import Test.QuickCheck

-- Property: reverse twice is identity
prop_reverse_reverse :: [Int] -> Bool
prop_reverse_reverse xs = reverse (reverse xs) == xs

-- Run test
quickCheck prop_reverse_reverse
-- +++ OK, passed 100 tests.

-- Property: sort is idempotent
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

-- Property: append associativity
prop_append_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_append_assoc xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
```

#### Custom Generators
```haskell
-- Generate positive integers
genPositive :: Gen Int
genPositive = abs <$> arbitrary `suchThat` (> 0)

-- Generate binary trees
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree
    where
      tree 0 = Leaf <$> arbitrary
      tree n = oneof
        [ Leaf <$> arbitrary
        , Branch <$> subtree <*> subtree
        ]
        where
          subtree = tree (n `div` 2)
```

#### Typeclass Laws
```haskell
-- Functor laws
prop_functor_id :: Tree Int -> Bool
prop_functor_id t = fmap id t == t

prop_functor_compose :: Fun Int Int -> Fun Int Int -> Tree Int -> Bool
prop_functor_compose (Fun _ f) (Fun _ g) t =
  fmap (f . g) t == (fmap f . fmap g) t

-- Monad laws
prop_monad_left_identity :: Int -> Fun Int (Maybe Int) -> Bool
prop_monad_left_identity x (Fun _ f) =
  (return x >>= f) == f x

prop_monad_right_identity :: Maybe Int -> Bool
prop_monad_right_identity m =
  (m >>= return) == m
```

### 10.2 Hspec (BDD-Style Testing)

**Readable test specifications:**

```haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1,2,3] `shouldBe` [3,2,1]
      
    it "is its own inverse" $ property $
      \xs -> reverse (reverse xs) == (xs :: [Int])
      
  describe "Foldable" $ do
    it "foldr and foldl give same result for commutative ops" $ property $
      \xs -> foldr (+) 0 xs == foldl (+) 0 (xs :: [Int])
      
  describe "Traversable" $ do
    it "validates all positive" $ do
      traverse validatePositive [1,2,3] `shouldBe` Just [1,2,3]
      
    it "fails on first negative" $ do
      traverse validatePositive [1,-2,3] `shouldBe` Nothing

validatePositive :: Int -> Maybe Int
validatePositive n
  | n > 0 = Just n
  | otherwise = Nothing
```

### 10.3 Test Organization

```haskell
-- test/Spec.hs
main :: IO ()
main = hspec $ do
  describe "User module" userSpec
  describe "Database module" databaseSpec

-- test/UserSpec.hs
userSpec :: Spec
userSpec = do
  describe "makeUser" $ do
    it "accepts valid user" $ do
      makeUser "Alice" "alice@example.com" 30
        `shouldSatisfy` isRight
        
    it "rejects invalid email" $ do
      makeUser "Alice" "invalidemail" 30
        `shouldSatisfy` isLeft
```

### 10.4 Coverage Requirements (from CURSOR.md)

**Mandatory**:
- ✅ **100% coverage** for pure functions
- ✅ **3+ tests per function** minimum
- ✅ All tests **must pass** before commit
- ✅ Property-based tests for pure functions
- ✅ Typeclass law testing

---

## 11. Build Tools (Stack)

**Stack: Reproducible builds** for Haskell.

### 11.1 Why Stack?

- ⭐⭐⭐⭐⭐ **Reproducible builds** (like Cargo for Rust)
- ⭐⭐⭐⭐⭐ **Isolated environments** (no dependency conflicts)
- ⭐⭐⭐⭐⭐ **Cross-platform** (Windows, macOS, Linux)
- ⭐⭐⭐⭐⭐ **LTS snapshots** (curated package sets)

### 11.2 Project Structure

```
my-project/
├── stack.yaml         # Stack configuration
├── package.yaml       # Package metadata (hpack format)
├── src/
│   └── Lib.hs
├── app/
│   └── Main.hs
├── test/
│   └── Spec.hs
└── .stack-work/       # Build artifacts (like target/ in Rust)
```

### 11.3 stack.yaml

```yaml
resolver: lts-21.17  # GHC 9.4.7

packages:
  - .

extra-deps: []
```

### 11.4 package.yaml (hpack)

```yaml
name: my-project
version: 0.1.0.0

dependencies:
  - base >= 4.7 && < 5
  - text
  - containers
  - aeson

library:
  source-dirs: src

executables:
  my-project-exe:
    main: Main.hs
    source-dirs: app
    dependencies:
      - my-project

tests:
  my-project-test:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - my-project
      - hspec
      - QuickCheck
```

### 11.5 Common Commands

```bash
# Initialize project
stack new my-project

# Build project
stack build

# Run executable
stack exec my-project-exe

# Run tests
stack test

# Interactive REPL
stack ghci

# Clean build artifacts
stack clean

# Install executable
stack install

# Check dependencies
stack ls dependencies
```

---

## 12. GHC Extensions

**Essential GHC language extensions.**

### 12.1 Essential Extensions (Always Use)

#### OverloadedStrings
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

-- String literals can be Text, ByteString, etc.
greeting :: Text
greeting = "Hello, World!"  -- No explicit conversion needed!
```

#### DeriveFunctor, DeriveFoldable, DeriveTraversable
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Now Tree is automatically a Functor, Foldable, and Traversable!
```

#### DeriveGeneric
```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

data User = User
  { userId :: Int
  , userName :: String
  } deriving (Generic, Show)

-- Automatic JSON instances!
instance ToJSON User
instance FromJSON User
```

### 12.2 Important Extensions (Use Often)

#### FlexibleContexts, FlexibleInstances
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- Allow more flexible type class constraints
processData :: (Monad m, MonadReader Config m, MonadError Error m) => Data -> m Result
```

#### TypeFamilies
```haskell
{-# LANGUAGE TypeFamilies #-}

class Collection c where
  type Elem c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool
```

#### GADTs
```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  Lit :: Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
```

### 12.3 Advanced Extensions (Use Sparingly)

#### RankNTypes
```haskell
{-# LANGUAGE RankNTypes #-}

-- Higher-rank polymorphism
runST :: (forall s. ST s a) -> a
```

#### TypeApplications
```haskell
{-# LANGUAGE TypeApplications #-}

-- Explicit type application
show @Int 42  -- "42"
read @Double "3.14"  -- 3.14
```

---

## 13. Real-World Example: REST API

**Type-safe REST API with Servant.**

### 13.1 Complete Servant Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- Domain types
data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

-- API definition (type-level!)
type UserAPI =
  "users" :> Get '[JSON] [User]                    -- GET /users
  :<|> "users" :> Capture "id" Int :> Get '[JSON] (Maybe User)  -- GET /users/:id
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User     -- POST /users
  :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()         -- DELETE /users/:id

-- In-memory database
type DB = Map.Map Int User

-- Server implementation
server :: IORef DB -> Server UserAPI
server dbRef = getUsers :<|> getUser :<|> createUser :<|> deleteUser
  where
    getUsers :: Handler [User]
    getUsers = do
      db <- liftIO $ readIORef dbRef
      return $ Map.elems db
    
    getUser :: Int -> Handler (Maybe User)
    getUser uid = do
      db <- liftIO $ readIORef dbRef
      return $ Map.lookup uid db
    
    createUser :: User -> Handler User
    createUser user = do
      liftIO $ modifyIORef dbRef (Map.insert (userId user) user)
      return user
    
    deleteUser :: Int -> Handler ()
    deleteUser uid = do
      liftIO $ modifyIORef dbRef (Map.delete uid)
      return ()

-- Run server
main :: IO ()
main = do
  -- Initialize database
  dbRef <- newIORef $ Map.fromList
    [ (1, User 1 "Alice" "alice@example.com")
    , (2, User 2 "Bob" "bob@example.com")
    ]
  
  putStrLn "Server running on port 8080"
  run 8080 (serve (Proxy :: Proxy UserAPI) (server dbRef))
```

### 13.2 Why Servant is Amazing

**Type-safe at compile time:**
- ✅ Routes verified by compiler
- ✅ Request/response types checked
- ✅ Automatic JSON serialization
- ✅ Generate client code automatically
- ✅ Generate documentation automatically

**This is unique to Haskell!** Other languages can't provide this level of type safety for web APIs.

---

## 14. Real-World Example: Parser

**Parser combinators with Parsec/Megaparsec.**

### 14.1 Complete Parser Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

-- AST for simple expression language
data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Eq, Show)

-- Lexer
lexer = makeTokenParser emptyDef

integer = fromInteger <$> integer lexer
parens = parens lexer
identifier = identifier lexer
whiteSpace = whiteSpace lexer

-- Expression parser
expr :: Parser Expr
expr = whiteSpace *> addExpr <* eof

addExpr :: Parser Expr
addExpr = chainl1 mulExpr (Add <$ char '+')

mulExpr :: Parser Expr
mulExpr = chainl1 term (Mul <$ char '*')

term :: Parser Expr
term = parens addExpr
   <|> Num <$> integer
   <|> Var <$> identifier

-- Parse function
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

-- Examples
-- parseExpr "42"           -- Right (Num 42)
-- parseExpr "x + 10"       -- Right (Add (Var "x") (Num 10))
-- parseExpr "(2 + 3) * 4"  -- Right (Mul (Add (Num 2) (Num 3)) (Num 4))

-- Evaluate
eval :: [(String, Int)] -> Expr -> Maybe Int
eval _ (Num n) = Just n
eval env (Var v) = lookup v env
eval env (Add e1 e2) = (+) <$> eval env e1 <*> eval env e2
eval env (Mul e1 e2) = (*) <$> eval env e1 <*> eval env e2

-- Example
-- eval [("x", 5)] (Add (Var "x") (Num 10))  -- Just 15
```

### 14.2 Why Parser Combinators are Great

**Compositional:**
- Small parsers combine into larger ones
- Type-safe
- Easy to test
- Monadic (use do-notation)

---

## 15. Data Structure Patterns

**Foldable and Traversable** (see full guide for details).

### 15.1 Quick Reference

**Foldable** (reduce/aggregate):
```haskell
-- Original definition
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b

-- Usage
sum [1,2,3]           -- 6
product [1,2,3]       -- 6
foldMap Sum [1,2,3]   -- Sum 6
```

**Traversable** (transform with effects):
```haskell
-- Original definition
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- Usage
traverse validatePositive [1,2,3]    -- Just [1,2,3]
traverse validatePositive [1,-2,3]   -- Nothing
```

**See**:
- [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md) - Full Haskell coverage (reference implementation!)
- [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md) - Quick reference for all 6 languages

---

## 16. Mandatory Rules Reference

**From CURSOR.md** - Universal rules that apply to Haskell projects:

### 16.1 Git Workflow (MANDATORY)
- ✅ **Commit every 30-60 min** (mandatory)
- ✅ Commit message follows template
- ✅ Update TODO list after each task

### 16.2 File Size Limits (MANDATORY)
- ✅ All files **< 250-300 lines** (mandatory)
- ✅ Split larger files into modules

### 16.3 Testing (MANDATORY)
- ✅ **100% coverage** for pure functions
- ✅ **3+ tests per function** minimum
- ✅ All tests **must pass** before commit
- ✅ Use QuickCheck for property-based tests
- ✅ Use Hspec for unit tests

### 16.4 Type Safety (MANDATORY)
- ✅ Type signatures on all top-level functions
- ✅ No partial functions (use Maybe/Either)
- ✅ No `undefined` or `error` in production code
- ✅ Use smart constructors for invariants

### 16.5 Documentation (MANDATORY)
- ✅ Haddock comments for public functions
- ✅ Examples in documentation
- ✅ README with setup instructions

### 16.6 Code Quality (MANDATORY)
- ✅ `hlint` clean (no warnings)
- ✅ `ghc -Wall` clean (no warnings)
- ✅ Formatted with `ormolu` or `brittany`

---

## Summary

**Haskell as Reference Implementation:**
- ⭐⭐⭐⭐⭐ Pure functional (gold standard)
- ⭐⭐⭐⭐⭐ Native HKT (no encoding!)
- ⭐⭐⭐⭐⭐ Lazy evaluation (infinite lists)
- ⭐⭐⭐⭐⭐ Type inference (Hindley-Milner)
- ⭐⭐⭐⭐⭐ Origin of typeclasses (Foldable, Traversable, Monad)
- ⭐⭐⭐⭐⭐ QuickCheck (property-based testing)
- ⭐⭐⭐⭐⭐ 30+ years mature ecosystem

**Use Haskell when:**
- Correctness is paramount
- Complex domain logic
- Type-safe APIs (Servant)
- Compilers/interpreters
- Financial systems
- Academic/research

**Build Tools**: Stack (reproducible builds)  
**Testing**: Hspec + QuickCheck (originated here!)  
**Web**: Servant (type-safe REST APIs)  

**See Also**:
- [CURSOR.md](CURSOR.md) - Mandatory universal rules
- [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md) - Full Foldable/Traversable coverage
- [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md) - Quick reference

---

**Haskell: The Reference Implementation for Functional Programming** 🎩







