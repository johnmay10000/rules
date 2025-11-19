---
title: Haskell Functional Programming Patterns Reference
language: haskell
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Haskell Functional Programming Patterns Reference

Quick reference for common functional programming patterns in Haskell. For detailed explanations, see `fp_style_guide.md`.

---

## Error Handling Patterns

### Either Type for Explicit Errors

Wrap success/failure in a type-safe container. By convention, `Left` is error, `Right` is success.

```haskell
-- Define error type
data DivisionError = DivideByZero | InvalidInput String
  deriving (Show, Eq)

-- Function that might fail
divide :: Double -> Double -> Either DivisionError Double
divide _ 0 = Left DivideByZero
divide x y = Right (x / y)

-- Usage
result1 = divide 10 2  -- Right 5.0
result2 = divide 10 0  -- Left DivideByZero

-- Transform with fmap
fmap (*2) result1  -- Right 10.0
fmap (*2) result2  -- Left DivideByZero

-- Chain with >>=
result3 = divide 10 2 >>= \x -> divide x 2  -- Right 2.5
```

**When to use**: Any operation that can fail. Replaces exceptions with type safety.

---

### Maybe Type for Optional Values

Represent values that might be absent.

```haskell
-- Safe head that returns Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Safe lookup in a list of key-value pairs
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((k,v):xs) 
  | key == k = Just v
  | otherwise = lookup key xs

-- Usage
result1 = safeHead [1,2,3]  -- Just 1
result2 = safeHead []       -- Nothing

-- Compose Maybe functions
getFirstUserEmail :: [(String, String)] -> Maybe String
getFirstUserEmail users = lookup "first" users >>= \email -> Just email
```

**When to use**: When a value might not exist. More lightweight than Either when no error details needed.

---

## Monadic Composition Patterns

### Do Notation for Sequential Operations

Syntactic sugar for monadic composition.

```haskell
-- Without do notation
processUser :: UserId -> Either Error User
processUser userId = 
  lookupUser userId >>= \user ->
  validateUser user >>= \validated ->
  Right validated

-- With do notation (much cleaner!)
processUser :: UserId -> Either Error User
processUser userId = do
  user <- lookupUser userId
  validated <- validateUser user
  return validated

-- Multiple bindings
createOrder :: UserId -> [ProductId] -> Either Error Order
createOrder userId productIds = do
  user <- lookupUser userId
  products <- traverse lookupProduct productIds
  validateInventory products
  order <- createOrderRecord user products
  return order
```

**When to use**: When chaining multiple monadic operations. Much more readable than nested binds.

---

### Monad Transformers for Combined Effects

Combine multiple monad effects (Reader, State, Error, IO).

```haskell
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- App monad combines Reader, Error, and IO
type App = ReaderT Config (ExceptT AppError IO)

data Config = Config { dbConnection :: String, apiKey :: String }
data AppError = NotFound | PermissionDenied

-- Run the app
runApp :: App a -> Config -> IO (Either AppError a)
runApp app config = runExceptT (runReaderT app config)

-- Usage in functions
getUser :: UserId -> App User
getUser userId = do
  config <- ask  -- From ReaderT
  liftIO $ putStrLn $ "Fetching user with API key: " ++ apiKey config
  result <- liftIO $ queryDB (dbConnection config) userId
  case result of
    Nothing -> throwE NotFound
    Just user -> return user
```

**When to use**: When you need multiple effects (configuration, errors, state) in one computation.

---

## Functor/Applicative Patterns

### Functor Map for Transformation

Apply a function to a value in a context.

```haskell
-- Double every element in a Maybe
doubleMaybe :: Maybe Int -> Maybe Int
doubleMaybe = fmap (*2)

-- Uppercase a string if it exists
uppercaseMaybe :: Maybe String -> Maybe String
uppercaseMaybe = fmap toUpper

-- Works with any Functor!
doubleList :: [Int] -> [Int]
doubleList = fmap (*2)

doubleEither :: Either Error Int -> Either Error Int
doubleEither = fmap (*2)
```

**When to use**: When you have a function for a plain value but need to apply it to a value in a context.

---

### Applicative Style for Multiple Arguments

Apply functions with multiple arguments to values in context.

```haskell
-- Add two Maybes
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my

-- Create a User from Maybe fields
data User = User String Int Email

createUser :: Maybe String -> Maybe Int -> Maybe Email -> Maybe User
createUser name age email = User <$> name <*> age <*> email

-- Validation example
validateForm :: Maybe String -> Maybe Int -> Maybe Email -> Maybe User
validateForm name age email = 
  User <$> (name >>= nonEmpty)
       <*> (age >>= \a -> if a > 0 then Just a else Nothing)
       <*> (email >>= validEmail)
  where
    nonEmpty s = if null s then Nothing else Just s
    validEmail e = if '@' `elem` e then Just e else Nothing
```

**When to use**: When you have a function of multiple arguments and want to apply it to values in a Functor context.

---

## Function Composition Patterns

### Function Composition with (.)

Compose functions right-to-left.

```haskell
-- Basic composition
addOne :: Int -> Int
addOne = (+1)

double :: Int -> Int
double = (*2)

-- Compose: double after addOne
addOneThenDouble :: Int -> Int
addOneThenDouble = double . addOne

-- Equivalent to: \x -> double (addOne x)
-- addOneThenDouble 5 = double (addOne 5) = double 6 = 12

-- Real example: process a list of strings
processStrings :: [String] -> [Int]
processStrings = map length . filter (not . null) . map trim
  where trim = dropWhile isSpace . dropWhileEnd isSpace

-- Point-free style (no explicit arguments)
sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map (^2)
```

**When to use**: To build complex functions from simple ones. More declarative than lambda.

---

### Left-to-Right Composition with (&)

Pipe operator for left-to-right composition.

```haskell
import Data.Function ((&))

-- Process a value through multiple steps
processData :: String -> Int
processData input = input
  & trim
  & words
  & filter (not . null)
  & map length
  & sum
  where trim = dropWhile isSpace . dropWhileEnd isSpace

-- Equivalent to: sum (map length (filter (not . null) (words (trim input))))

-- With error handling
processUserData :: String -> Either Error User
processUserData raw = Right raw
  & fmap parseJSON
  & join  -- Flatten Either Error (Either Error User)
  & bind validateUser
  & bind enrichUserData
```

**When to use**: When you want to express a pipeline of transformations in reading order.

---

## Immutable Data Patterns

### Newtype for Type Safety

Create distinct types that share representation.

```haskell
-- BAD: Prone to mixups
sendEmail :: String -> String -> IO ()
sendEmail address body = ...

-- Can accidentally swap arguments!
-- sendEmail "body text" "user@example.com"  -- Compiles but wrong!

-- GOOD: Type-safe newtypes
newtype EmailAddress = EmailAddress String
newtype EmailBody = EmailBody String

sendEmail :: EmailAddress -> EmailBody -> IO ()
sendEmail (EmailAddress addr) (EmailBody body) = ...

-- Usage
addr = EmailAddress "user@example.com"
body = EmailBody "Hello, world!"
sendEmail addr body  -- Type-safe!

-- Can't swap anymore!
-- sendEmail body addr  -- TYPE ERROR!
```

**When to use**: For domain-specific types to prevent mixing up values of same representation.

---

### Record Update Syntax

Immutable updates for records.

```haskell
data User = User 
  { userId :: Int
  , userName :: String
  , userEmail :: String
  , userAge :: Int
  } deriving (Show)

-- Create a user
john :: User
john = User 1 "John" "john@example.com" 30

-- Update age (creates new User, original unchanged)
olderJohn :: User
olderJohn = john { userAge = 31 }

-- Update multiple fields
movedJohn :: User
movedJohn = john 
  { userEmail = "john.new@example.com"
  , userAge = 31
  }

-- Original unchanged
-- john == User 1 "John" "john@example.com" 30
-- olderJohn == User 1 "John" "john@example.com" 31
```

**When to use**: For immutable updates to record types. Creates new values, leaves original unchanged.

---

## Foldable/Traversable Patterns

### Fold for Aggregation

Combine elements using a binary operation.

```haskell
-- Sum a list
total :: [Int] -> Int
total = foldr (+) 0

-- Product
product :: [Int] -> Int
product = foldr (*) 1

-- Custom aggregation
data Order = Order { orderItems :: [Item], orderDiscount :: Double }
data Item = Item { itemPrice :: Double, itemQuantity :: Int }

orderTotal :: Order -> Double
orderTotal order = 
  let subtotal = foldr (\item acc -> acc + itemPrice item * fromIntegral (itemQuantity item)) 0 (orderItems order)
  in subtotal * (1 - orderDiscount order)

-- Fold with early termination
find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr (\x acc -> if p x then Just x else acc) Nothing

-- Fold for boolean operations
allEven :: [Int] -> Bool
allEven = foldr (&&) True . map even

anyLarge :: [Int] -> Bool
anyLarge = foldr (||) False . map (>1000)
```

**When to use**: To reduce a collection to a single value (sum, product, find, etc.).

---

### Traverse for Effectful Transformations

Apply a function that returns a monad to each element, collecting results.

```haskell
-- Validate all elements
validateAll :: [Int] -> Either Error [Int]
validateAll = traverse validatePositive
  where
    validatePositive x = if x > 0 then Right x else Left (NegativeError x)

-- Usage
validateAll [1,2,3]  -- Right [1,2,3]
validateAll [1,-2,3] -- Left (NegativeError -2)

-- Parse all strings to integers
parseAll :: [String] -> Maybe [Int]
parseAll = traverse readMaybe
  where
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

-- Usage
parseAll ["1","2","3"]  -- Just [1,2,3]
parseAll ["1","abc","3"] -- Nothing

-- Load all users in parallel (if IO supports it)
loadAllUsers :: [UserId] -> IO [User]
loadAllUsers = traverse loadUser
```

**When to use**: When you have a function `a -> m b` and a `[a]`, and want `m [b]`. Fails fast (stops at first error).

---

## Lens Patterns

### Lens for Immutable Updates

Focus on parts of data structures.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

-- Define data with lenses
data User = User 
  { _userName :: String
  , _userEmail :: String
  , _userAddress :: Address
  } deriving (Show)

data Address = Address
  { _addressStreet :: String
  , _addressCity :: String
  , _addressZip :: String
  } deriving (Show)

-- Generate lenses automatically
makeLenses ''User
makeLenses ''Address

-- Usage
user :: User
user = User "Alice" "alice@example.com" 
            (Address "123 Main St" "NYC" "10001")

-- Update nested field
updatedUser :: User
updatedUser = user & userAddress . addressCity .~ "Boston"

-- View nested field
city :: String
city = user ^. userAddress . addressCity  -- "NYC"

-- Compose transformations
movedUser :: User
movedUser = user
  & userAddress . addressCity .~ "Los Angeles"
  & userAddress . addressStreet .~ "456 Hollywood Blvd"
  & userName .~ "Alice Smith"

-- Work with Maybe fields
data UserProfile = UserProfile
  { _profilePhone :: Maybe String
  , _profileWebsite :: Maybe String
  } deriving (Show)

makeLenses ''UserProfile

profile :: UserProfile
profile = UserProfile (Just "555-1234") Nothing

-- Safe update
updatedProfile :: UserProfile
updatedProfile = profile & profilePhone . _Just .~ "555-5678"

-- Safe access
phone :: Maybe String
phone = profile ^? profilePhone . _Just  -- Just "555-1234"
```

**When to use**: For deep immutable updates to nested records. Much cleaner than manual record update syntax.

---

## Parser Combinator Patterns

### Building Parsers with Combinators

Compose small parsers into complex ones.

```haskell
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

-- Basic parsers
digitParser :: Parser Char
digitParser = digitChar

numberParser :: Parser Int
numberParser = read <$> some digitChar

-- Combine with <|> (choice)
boolParser :: Parser Bool
boolParser = (True <$ string "true") <|> (False <$ string "false")

-- Sequence with do notation
data User = User String Int

userParser :: Parser User
userParser = do
  name <- some letterChar
  spaceChar
  age <- numberParser
  return $ User name age

-- Parse "Alice 30" -> User "Alice" 30

-- More complex: JSON-like parser
data JsonValue = JsonString String | JsonNumber Double | JsonBool Bool | JsonNull

jsonValueParser :: Parser JsonValue
jsonValueParser = jsonStringParser <|> jsonNumberParser <|> jsonBoolParser <|> jsonNullParser
  where
    jsonStringParser = JsonString <$> (char '"' *> manyTill anySingle (char '"'))
    jsonNumberParser = JsonNumber . read <$> some (digitChar <|> char '.')
    jsonBoolParser = JsonBool <$> boolParser
    jsonNullParser = JsonNull <$ string "null"

-- Run parser
parseUser :: String -> Either (ParseErrorBundle String Void) User
parseUser = parse userParser "user"
```

**When to use**: For parsing structured text. Composable, type-safe, and easier to maintain than regex or manual parsing.

---

## Advanced Patterns

### Reader Monad for Dependency Injection

Pass configuration implicitly.

```haskell
-- App monad with configuration
type App = Reader Config

data Config = Config 
  { configDb :: Database
  , configLogger :: Logger
  , configApiKey :: String
  }

-- Functions get config automatically
getUser :: UserId -> App (Maybe User)
getUser userId = do
  db <- asks configDb
  return $ queryDB db userId

createOrder :: UserId -> [Product] -> App Order
createOrder userId products = do
  logger <- asks configLogger
  let logMsg = "Creating order for user: " ++ show userId
  return $ Order userId products

-- Run with specific config
runApp :: App a -> Config -> a
runApp app config = runReader app config

-- Usage
config :: Config
config = Config myDatabase myLogger "api-key-123"

user = runApp (getUser 1) config
```

**When to use**: For passing configuration or shared dependencies through call chains without explicit parameters.

---

### State Monad for Stateful Computations

Thread state through computations implicitly.

```haskell
-- Game state
type Game = State GameState

data GameState = GameState
  { gsPlayerPos :: (Int, Int)
  , gsScore :: Int
  , gsInventory :: [Item]
  } deriving (Show)

data Item = Sword | Potion | Key deriving (Show)

-- State transformations
movePlayer :: (Int, Int) -> Game ()
movePlayer delta = modify $ \s -> s { gsPlayerPos = addPos (gsPlayerPos s) delta }
  where addPos (x,y) (dx,dy) = (x+dx, y+dy)

collectItem :: Item -> Game ()
collectItem item = modify $ \s -> s { gsInventory = item : gsInventory s }

addScore :: Int -> Game ()
addScore points = modify $ \s -> s { gsScore = gsScore s + points }

-- Complex action
findTreasure :: Game ()
findTreasure = do
  movePlayer (1, 1)
  collectItem Sword
  addScore 100

-- Run game
initialState :: GameState
initialState = GameState (0,0) 0 []

finalState :: GameState
finalState = execState findTreasure initialState
-- GameState {gsPlayerPos = (1,1), gsScore = 100, gsInventory = [Sword]}
```

**When to use**: For computations with mutable state that you want to keep pure and composable.

---

## Performance Patterns

### Strictness Annotations

Control evaluation order to avoid space leaks.

```haskell
-- Lazy (default)
sumLazy :: [Int] -> Int
sumLazy = foldr (+) 0
-- Builds thunk: 1 + (2 + (3 + (4 + 0)))

-- Strict fold
sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0
-- Evaluates immediately: (((0 + 1) + 2) + 3) + 4

-- Strict fields
data Point = Point !Double !Double
-- x and y are evaluated to WHNF when Point is created

-- Bang patterns
processList :: [Int] -> Int
processList xs = go 0 xs
  where
    go !acc [] = acc  -- acc is strict
    go !acc (x:xs) = go (acc + x) xs

-- seq for strict evaluation
deepSeqList :: [Int] -> ()
deepSeqList xs = foldr seq () xs
```

**When to use**: When lazy evaluation causes space leaks or performance issues. Use strict folds for numeric accumulations.

---

## Testing Patterns

### Property-Based Testing with QuickCheck

Test properties instead of specific cases.

```haskell
import Test.QuickCheck

-- Property: reverse is involutive
prop_reverseInvolutive :: [Int] -> Bool
prop_reverseInvolutive xs = reverse (reverse xs) == xs

-- Property: sort is idempotent
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs

-- Property: addition is commutative
prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

-- Property with implication
prop_divideBySelf :: Double -> Property
prop_divideBySelf x = x /= 0 ==> divide x x == Right 1

-- Custom generators
genUser :: Gen User
genUser = User <$> arbitrary <*> arbitrary <*> arbitrary

-- Run tests
main :: IO ()
main = do
  quickCheck prop_reverseInvolutive
  quickCheck prop_sortIdempotent
  quickCheck prop_addCommutative
```

**When to use**: To test invariants and properties that should hold for all inputs. Finds edge cases automatically.

---

## Summary

These patterns form the core of functional programming in Haskell:

1. **Error Handling**: Use `Either` for errors, `Maybe` for optional values
2. **Composition**: Use `do` notation or `>>=` for monadic composition
3. **Transformation**: Use `fmap` for mapping, `<$>`/`<*>` for applicative
4. **Reduction**: Use `foldr`/`foldl'` for aggregating collections
5. **Traversal**: Use `traverse` for effectful transformations
6. **Updates**: Use lenses for deep immutable updates
7. **Testing**: Use QuickCheck for property-based testing

**Key Principle**: Compose small, pure functions into larger computations. Let the type system guide you.

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global Rules System  
**Status**: Active