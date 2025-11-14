# Haskell Functional Programming Style Guide for Kimi CLI

**Purpose**: Comprehensive Haskell functional programming guide for Kimi CLI  
**Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Status**: Active - Production Ready  
**Scope**: Haskell projects using GHC 9.6+ and Stack  

---

## Quick Links

**Essential Reading**:
- [KIMI.md](KIMI.md) - Mandatory universal rules
- [cursor/guides/traversable-foldable-guide.md](cursor/guides/traversable-foldable-guide.md) - Full Foldable/Traversable coverage (Haskell as reference!)
- [cursor/DATA_STRUCTURE_PATTERNS.md](cursor/DATA_STRUCTURE_PATTERNS.md) - Quick reference

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

---

## 1. Why Haskell: The Reference Implementation

### Haskell's Unique Position

**Haskell is where functional programming concepts originated**. When we talk about Foldable, Traversable, Monads, and other FP patterns in Python, TypeScript, Rust, Kotlin, or Swift, we're talking about implementations of Haskell concepts.

**Key Positioning**:
- ⭐⭐⭐⭐⭐ **Reference Implementation** - The original definitions
- ⭐⭐⭐⭐⭐ **Pure Functional** - Purity enforced by the type system
- ⭐⭐⭐⭐⭐ **Native HKT** - Full Higher-Kinded Types (no encoding!)
- ⭐⭐⭐⭐⭐ **Lazy by Default** - Call-by-need evaluation
- ⭐⭐⭐⭐⭐ **Type Inference** - Strong types, minimal annotations
- ⭐⭐⭐⭐⭐ **30+ Years Mature** - Battle-tested ecosystem

### What Makes Haskell Special

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

-- Other languages:
-- - Python: itertools.islice (clumsy)
-- - TypeScript: Generator functions (syntax overhead)
-- - Rust: Iterators (explicit)
```

---

## Kimi-Specific Patterns

### Parallel Validation of Haskell Code

Kimi can validate multiple Haskell functions and type signatures simultaneously:

```haskell
-- Kimi excels at validating Haskell's type system and purity
-- All validations can run in parallel

data User = User
  { userId :: UserId
  , userName :: Text
  , userEmail :: Email
  } deriving (Eq, Show)

-- Kimi validates simultaneously:
-- ✓ Type signatures
-- ✓ Record syntax correctness
-- ✓ Deriving clauses
-- ✓ Purity of functions

validateUser :: User -> Maybe ValidationError
validateUser user = 
  validateEmail (userEmail user)  -- Kimi validates this call
    *> validateName (userName user)  -- And this one (parallel)
    *> Just Nothing  -- And composition

updateName :: Text -> User -> User
updateName newName user = user { userName = newName }
-- Kimi validates: ✓ Record update syntax
--                 ✓ Immutability
--                 ✓ Returns new value
```

### Subagent Pattern for Complex Type Checking

Use Kimi's Task tool to spawn subagents for validating complex type-level programming:

```haskell
-- Complex type-level validation benefits from Kimi's subagents
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

type family Validate (schema :: Schema) (a :: *) :: ValidationResult where
  Validate SchemaUser User = 'Validated
  Validate _ _ = 'NotValidated

-- Kimi can spawn subagents to verify:
-- - Type family correctness
-- - DataKinds usage
-- - Type-level computations
-- - GHC extension compatibility

data ValidationResult = Validated | NotValidated
```

---

## 2. Core FP Principles

### Referential Transparency
```haskell
-- The same input ALWAYS gives the same output
-- No side effects, no hidden state

double :: Int -> Int
double x = x * 2

-- Can replace any call with its result (referential transparency)
-- double 5  ===>  10
-- 10        ===>  10
```

### Immutability
```haskell
-- All data is immutable by default
user :: User
user = User "123" "Alice" "alice@example.com"

-- "Updating" creates new value
updatedUser :: User
updatedUser = user { userEmail = "new@example.com" }
-- user is STILL unchanged!
```

### First-Class Functions
```haskell
-- Functions are values
add :: Int -> Int -> Int
add = (+)

-- Partial application
add5 :: Int -> Int
add5 = add 5

-- Higher-order functions
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- Usage
result = map double [1,2,3,4,5]  -- [2,4,6,8,10]
```

---

## 3. Type System

### Algebraic Data Types (ADTs)
```haskell
-- Product types (AND)
data Point = Point Double Double
-- Point has x AND y

-- Sum types (OR)
data Maybe a = Nothing | Just a
-- Maybe has Nothing OR Just a

data Either e a = Left e | Right a
-- Either has Left e OR Right a

-- Complex ADTs
data Tree a = Empty | Node a (Tree a) (Tree a)
-- Tree is Empty OR Node with value AND left AND right subtrees
```

### Newtype Pattern (Type Safety)
```haskell
-- Distinct types that share representation
newtype UserId = UserId Text deriving (Eq, Show)
newtype Email = Email Text deriving (Eq, Show)

-- Type-safe! Can't mix up
sendEmail :: Email -> Text -> IO ()
sendEmail (Email addr) body = ...

-- Wrong: sendEmail "not-an-email" "body"
-- Right: sendEmail (Email "user@example.com") "body"
```

### Type Inference
```haskell
-- Type annotations often optional
-- Compiler infers most types

-- Explicit
doubleExplicit :: Int -> Int
doubleExplicit x = x * 2

-- Inferred
doubleInferred x = x * 2  -- Infers: doubleInferred :: Num a => a -> a

-- Polymorphic
identity x = x  -- Infers: identity :: a -> a
```

---

## 4. Typeclasses

### Functor
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- All Functors must satisfy laws:
-- 1. fmap id = id                    (identity)
-- 2. fmap (g . f) = fmap g . fmap f  (composition)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor [] where
  fmap = map

-- Usage
fmap (+1) (Just 5)  -- Just 6
fmap (+1) Nothing   -- Nothing
fmap (+1) [1,2,3,4] -- [2,3,4,5]
```

### Applicative
```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f <*> x = fmap f x

-- Usage
pure 5 :: Maybe Int  -- Just 5
Just (+1) <*> Just 5  -- Just 6
Just (+) <*> Just 5 <*> Just 3  -- Just 8
Nothing <*> Just 5   -- Nothing
```

### Monad
```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b  -- "bind"
  return :: a -> m a
  
  return = pure

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= f = f x

-- Usage (do notation)
do
  x <- Just 5
  y <- Just 3
  return (x + y)  -- Just 8

-- Desugared
Just 5 >>= (\x ->
  Just 3 >>= (\y ->
    return (x + y)))
```

### Foldable and Traversable
```haskell
-- See cursor/guides/traversable-foldable-guide.md for comprehensive coverage!

-- Foldable: Combine elements
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  
instance Foldable [] where
  foldMap f = mconcat . map f

-- Traversable: Traverse with effects
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  
instance Traversable [] where
  traverse _ [] = pure []
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs
```

---

## 5. Monad Transformers

```haskell
-- Combine monad effects

type App = ReaderT Env (ExceptT Error (StateT State IO))
-- App a = Env -> Error -> State -> IO (Error, State, a)

-- Run the monad stack
runApp :: App a -> Env -> IO (Either Error (a, State))
runApp app env = do
  ((result, state), _) <- 
    runStateT (runExceptT (runReaderT app env)) initialState
  return result

-- Or individual transformers
maybeResult :: MaybeT IO Int
maybeResult = do
  x <- liftIO (readLn :: IO Int)
  guard (x > 0)
  return x
```

---

## 6. Error Handling

### Using Either
```haskell
-- FP error handling (not exceptions!)
data ValidationError = InvalidEmail | EmptyName
type ValidationResult = Either ValidationError

validateUser :: User -> ValidationResult User
validateUser user = do
  validateEmail (userEmail user)
  validateName (userName user)
  return user

validateEmail :: Text -> ValidationResult ()
validateEmail email
  | isValidEmail email = Right ()
  | otherwise = Left InvalidEmail
```

### Using Maybe
```haskell
-- For optional values
findUser :: UserId -> Maybe User
findUser userId = lookup userId userDatabase

-- Safe composition
getUserEmail :: UserId -> Maybe Email
getUserEmail userId = do
  user <- findUser userId
  return (userEmail user)

-- Or: getUserEmail = fmap userEmail . findUser
```

---

## 7. Common Libraries

```yaml
# stack.yaml or cabal file

# Core FP
- mtl  # Monad transformers
- lens  # Optics (powerful!)
- aeson  # JSON
- servant  # Type-safe web APIs
- persistent  # Type-safe DB
- optparse-applicative  # CLI parsing
- hedgehog  # Property testing
- quickcheck  # Property testing

-- Example imports
import Control.Lens ((^.), (.~), (%~), at, ix)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Servant (Get, Post, (:>), (:<|>)(..), Server)
```

---

## 8. Testing Patterns

### Property-Based Testing
```haskell
import Test.QuickCheck

-- Property: reverse is involutive
prop_reverseInvolutive :: [Int] -> Bool
prop_reverseInvolutive xs = reverse (reverse xs) == xs

-- Property: sort is idempotent
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs

-- Custom generators
genUser :: Gen User
genUser = User <$> arbitrary <*> arbitrary <*> arbitrary

-- Run tests
main :: IO ()
main = do
  quickCheck prop_reverseInvolutive
  quickCheck prop_sortIdempotent
```

### Kimi-Specific Testing Notes
- Kimi can spawn subagents to run test suites in parallel  
- Validate QuickCheck properties cover edge cases
- Verify hedgehog generators are comprehensive

---

## 9. Real-World Example: Type-Safe API

```haskell
{-# LANGUAGE DataKinds, TypeOperators #-}
import Servant

-- Type-safe API specification
type UserAPI = 
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "userId" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId

-- Server implementation
server :: Server UserAPI
server = getUsers :<|> getUser :<|> createUser

getUsers :: Handler [User]
getUsers = do
  -- Kimi validates: ✓ Error handling
  --                 ✓ Type safety
  --                 ✓ JSON encoding
  users <- liftIO (queryUserDatabase)
  return users

getUser :: UserId -> Handler User
getUser userId = do
  maybeUser <- liftIO (findUserById userId)
  case maybeUser of
    Nothing -> throwError err404
    Just user -> return user

createUser :: User -> Handler UserId
createUser user = do
  userId <- liftIO (insertUser user)
  return userId
```

---

## 10. Kimi CLI Integration

### Project Setup

1. **Environment Variable**:
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   export KIMI_HASKELL_RULES="$KIMI_RULES_PATH/kimi/haskell-fp-style-guide.md"
   ```

2. **Project Rules** (`.kimirules`):
   ```markdown
   # .kimirules for Haskell Project
   
   ## Haskell FP Rules
   - No partial functions (use total functions with Maybe/Either)
   - Explicit type signatures for all top-level functions
   - No IO in pure code (use Monad transformers)
   - Property tests for all invariants (QuickCheck/Hedgehog)
   - Lens for record updates (not partial field updates)
   
   ## Kimi-Specific
   - Use Task tool to validate type signatures across modules
   - Spawn subagents for GHC extension compatibility check
   - Parallel type-checking with ghci across files
   - Validate all typeclass instances follow laws
   - Check for exhaustive pattern matching
   ```

3. **Stack Integration**:
   ```yaml
   # stack.yaml
   ghc-options:
     "$everything": -Wall -Werror -Wincomplete-patterns
   ```

4. **Validation Commands** (Kimi can run in parallel):
   ```bash
   stack build --fast --pedantic  # Fast build with warnings as errors
   stack test --coverage          # Run tests with coverage
   stack exec -- hlint .         # Run hlint for code quality
   stack ghci --ghci-options=-Wall # Type-check all files
   ```

---

## Summary

Haskell is the reference implementation for functional programming. Its pure functions, higher-kinded types, and lazy evaluation provide the foundation for FP concepts used in all other languages. Use explicit error handling with Either/Maybe, compose small pure functions, and leverage the powerful type system.

**Key Takeaways**:
- Pure functions ONLY (tracked by type system)
- Higher-Kinded Types (native, no encoding)
- Lazy evaluation by default
- Algebraic Data Types for domain modeling
- Typeclasses for ad-hoc polymorphism
- Monad transformers for effect composition
- Kimi's parallel validation for multi-file type-checking

**Why This Matters**: All other language FP guides in this repository (Python, TypeScript, Rust, Kotlin, Swift) are implementations of Haskell concepts. Understanding Haskell provides deep insight into FP patterns everywhere.

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for mapping Haskell concepts to other languages.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
