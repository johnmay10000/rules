# Haskell Testing Tools Research

**Date**: 2025-11-01  
**Purpose**: Research Haskell testing frameworks for guide  
**Phase**: Phase 0 - Planning & Research  

---

## Overview

Haskell has excellent testing tools, particularly for property-based testing (QuickCheck originated here!). Need to choose frameworks for guide examples.

---

## Option 1: QuickCheck ⭐⭐⭐⭐⭐ (MUST INCLUDE!)

### What is QuickCheck?

**Property-based testing** - Test properties that should always hold

**Key Features**:
- ⭐⭐⭐⭐⭐ **Originated in Haskell** (1999) - Unique strength!
- ⭐⭐⭐⭐⭐ Property-based testing
- ⭐⭐⭐⭐⭐ Automatic test case generation
- ⭐⭐⭐⭐⭐ Shrinking (finds minimal failing case)
- ⭐⭐⭐⭐⭐ Widely adopted (industry standard)

### Example

```haskell
import Test.QuickCheck

-- Property: reverse twice is identity
prop_reverse_reverse :: [Int] -> Bool
prop_reverse_reverse xs = reverse (reverse xs) == xs

-- Property: sort is idempotent
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

-- Property: append associativity
prop_append_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_append_assoc xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Run tests
main = do
  quickCheck prop_reverse_reverse
  quickCheck prop_sort_idempotent
  quickCheck prop_append_assoc
```

**Output**:
```
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
```

### Why QuickCheck is Essential

**Pros**:
- ✅ **Originated in Haskell** - This is where it was invented!
- ✅ Shows Haskell's research heritage
- ✅ More powerful than example-based testing
- ✅ Finds edge cases automatically
- ✅ Shrinking finds minimal failing case

**Cons**:
- ❌ Requires thinking in properties (learning curve)

**Decision**: **MUST INCLUDE** - Unique Haskell strength!

---

## Option 2: Hspec ⭐⭐⭐⭐⭐ (RECOMMENDED)

### What is Hspec?

**BDD-style testing framework** (like RSpec for Ruby, Jasmine for JS)

**Key Features**:
- Readable test specifications
- describe/it structure
- Good for unit tests
- Integrates with QuickCheck
- Good for beginners

### Example

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
      
  describe "addition" $ do
    it "is commutative" $ property $
      \x y -> x + y == y + (x :: Int)
```

**Output**:
```
reverse
  reverses a list
  is its own inverse
    +++ OK, passed 100 tests.

addition
  is commutative
    +++ OK, passed 100 tests.

Finished in 0.0123 seconds
3 examples, 0 failures
```

### Why Hspec?

**Pros**:
- ✅ Readable specs (describe/it)
- ✅ Integrates with QuickCheck
- ✅ Good for unit tests
- ✅ Familiar to developers from other languages

**Cons**:
- ❌ None (very good!)

**Decision**: **YES - Use Hspec** (with QuickCheck integration)

---

## Option 3: Tasty ⭐⭐⭐⭐

### What is Tasty?

**Test framework aggregator** - Run multiple test types together

**Key Features**:
- Supports QuickCheck, HUnit, SmallCheck
- Unified test runner
- Parallel test execution
- Flexible test organization

### Example

```haskell
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Unit tests"
      [ testCase "List comparison" $
          [1,2,3] @?= [1,2,3]
      ]
  , testGroup "Properties"
      [ testProperty "reverse twice" $
          \xs -> reverse (reverse xs) == (xs :: [Int])
      ]
  ]
```

### Why Not Tasty for Guide?

**Pros**:
- ✅ Flexible and modular
- ✅ Good for large projects

**Cons**:
- ❌ More complex than needed
- ❌ Hspec + QuickCheck is simpler

**Decision**: **NO - Hspec + QuickCheck is sufficient**

---

## Option 4: HUnit ⭐⭐⭐

### What is HUnit?

**Unit testing framework** (like JUnit, pytest)

**Key Features**:
- Simple assertions
- Good for example-based tests
- Lightweight

### Example

```haskell
import Test.HUnit

tests = TestList
  [ TestCase (assertEqual "reverse" [3,2,1] (reverse [1,2,3]))
  , TestCase (assertEqual "sum" 6 (sum [1,2,3]))
  ]

main = runTestTT tests
```

### Why Not HUnit for Guide?

**Pros**:
- ✅ Simple and straightforward

**Cons**:
- ❌ Less powerful than Hspec
- ❌ No QuickCheck integration
- ❌ Hspec is better for beginners

**Decision**: **NO - Hspec is better**

---

## Final Testing Strategy

### Primary: Hspec + QuickCheck ⭐⭐⭐⭐⭐

**Use Hspec as main framework**:
- Readable specifications
- Good structure (describe/it)
- Easy for beginners

**Integrate QuickCheck**:
- Use `property` helper in Hspec
- Show property-based testing
- Emphasize Haskell's strength

### Example Structure

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  
  -- Unit tests
  describe "foldl" $ do
    it "sums a list" $ do
      foldl (+) 0 [1,2,3] `shouldBe` 6
      
  -- Property-based tests
  describe "Properties" $ do
    it "reverse is idempotent" $ property $
      \xs -> reverse (reverse xs) == (xs :: [Int])
      
    it "map preserves length" $ property $
      \xs -> length (map (+1) xs) == length (xs :: [Int])
      
    it "foldr and foldl give same result for commutative ops" $ property $
      \xs -> foldr (+) 0 xs == foldl (+) 0 (xs :: [Int])
```

**This showcases**:
- Readable test structure (Hspec)
- Property-based testing (QuickCheck)
- Haskell's testing strength
- Both unit and property tests

---

## Libraries Needed

### Core Testing
```yaml
dependencies:
  - hspec
  - QuickCheck
```

### Optional (Advanced)
```yaml
dependencies:
  - hspec-discover  # Auto-discover tests
  - quickcheck-instances  # More Arbitrary instances
```

**Decision**: Include core only

---

## Testing Patterns to Cover

### 1. Property-Based Testing ⭐⭐⭐⭐⭐

**Must emphasize** - Originated in Haskell!

```haskell
-- Laws that should always hold
prop_functor_identity :: Tree Int -> Bool
prop_functor_identity tree = fmap id tree == tree

prop_functor_composition :: Fun Int Int -> Fun Int Int -> Tree Int -> Bool
prop_functor_composition (Fun _ f) (Fun _ g) tree =
  fmap (f . g) tree == (fmap f . fmap g) tree
```

### 2. Typeclass Laws

```haskell
-- Monad laws
prop_monad_left_identity :: Int -> Fun Int (Maybe Int) -> Bool
prop_monad_left_identity x (Fun _ f) =
  (return x >>= f) == f x

prop_monad_right_identity :: Maybe Int -> Bool
prop_monad_right_identity m =
  (m >>= return) == m

prop_monad_associativity :: Maybe Int -> Fun Int (Maybe Int) -> Fun Int (Maybe Int) -> Bool
prop_monad_associativity m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
```

### 3. Custom Generators

```haskell
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

---

## Coverage Requirements

### Mandatory (from CURSOR.md)
- ✅ **100% coverage** for pure functions
- ✅ **3+ tests per function** minimum
- ✅ All tests **must pass** before commit

### Haskell-Specific
- ✅ Property-based tests for pure functions
- ✅ Typeclass law testing
- ✅ Custom generators for domain types
- ✅ Shrinking for minimal failing cases

---

## Summary

**Testing Framework**: **Hspec + QuickCheck** ⭐⭐⭐⭐⭐

**Why This Combination**:
1. ⭐⭐⭐⭐⭐ **QuickCheck** - Originated in Haskell (unique strength!)
2. ⭐⭐⭐⭐⭐ **Hspec** - Readable, integrates QuickCheck, good for beginners
3. ⭐⭐⭐⭐⭐ **Property-based** - More powerful than example-based
4. ⭐⭐⭐⭐⭐ **Typeclass Laws** - Test Functor, Monad laws
5. ⭐⭐⭐⭐⭐ **Industry Standard** - Used in production

**Example Coverage**:
- Unit tests (Hspec describe/it)
- Property-based tests (QuickCheck properties)
- Typeclass law testing
- Custom generators

**Libraries**: hspec, QuickCheck

---

**Testing Tools Research Complete!** Ready for implementation strategy. 🎩

