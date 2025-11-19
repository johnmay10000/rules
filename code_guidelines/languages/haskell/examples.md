---
title: Haskell Functional Programming Examples
language: haskell
category: code_guidelines
type: examples
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Haskell Functional Programming Examples

Comprehensive code examples demonstrating functional programming patterns in Haskell.

---

## Example 1: Error Handling with Either/Maybe

### Before (Imperative with Exceptions)

```haskell
-- BAD: Uses partial functions and exceptions
divideUnsafe :: Double -> Double -> Double
divideUnsafe _ 0 = error "Cannot divide by zero"  -- Throws exception!
divideUnsafe x y = x / y

headUnsafe :: [a] -> a
headUnsafe [] = error "Empty list!"  -- Throws exception!
headUnsafe (x:_) = x

-- Usage (can crash program)
result1 = divideUnsafe 10 2  -- 5.0
-- result2 = divideUnsafe 10 0  -- CRASH!
-- result3 = headUnsafe []  -- CRASH!
```

### After (Functional with Result Types)

```haskell
-- GOOD: Type-safe error handling
data DivisionError = DivideByZero | InvalidInput String
  deriving (Show, Eq)

-- Explicit error handling with Either
divideSafe :: Double -> Double -> Either DivisionError Double
divideSafe _ 0 = Left DivideByZero
divideSafe x y = Right (x / y)

-- Safe head with Maybe
headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

-- Usage
result1 :: Either DivisionError Double
result1 = divideSafe 10 2  -- Right 5.0

result2 :: Either DivisionError Double
result2 = divideSafe 10 0  -- Left DivideByZero

result3 :: Maybe Int
result3 = headSafe [1,2,3]  -- Just 1

result4 :: Maybe Int
result4 = headSafe []  -- Nothing

-- Chain operations safely
calculate :: Double -> Double -> Either DivisionError Double
calculate x y = do
  quotient <- divideSafe x y
  return (quotient * 2)

-- Usage
calc1 = calculate 10 2  -- Right 10.0
calc2 = calculate 10 0  -- Left DivideByZero
```

---

## Example 2: Monadic Composition for Validation

### Scenario: User Registration Validation Pipeline

```haskell
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import Data.Char (isDigit)

-- Domain types
data Email = Email String deriving (Show, Eq)
data Password = Password String deriving (Show, Eq)
data User = User 
  { userName :: String
  , userEmail :: Email
  , userPassword :: Password
  , userAge :: Int
  } deriving (Show)

-- Error types
data ValidationError
  = EmailInvalid String
  | PasswordTooShort Int
  | PasswordNoNumber
  | AgeTooYoung Int
  | FieldEmpty String
  deriving (Show, Eq)

-- Validation functions
validateEmail :: String -> Either ValidationError Email
validateEmail s
  | null s = Left (FieldEmpty "email")
  | '@' `elem` s = Right (Email s)
  | otherwise = Left (EmailInvalid s)

validatePassword :: String -> Either ValidationError Password
validatePassword s
  | null s = Left (FieldEmpty "password")
  | length s < 8 = Left (PasswordTooShort (length s))
  | not (any isDigit s) = Left PasswordNoNumber
  | otherwise = Right (Password s)

validateAge :: Int -> Either ValidationError Int
validateAge a
  | a < 18 = Left (AgeTooYoung a)
  | otherwise = Right a

validateName :: String -> Either ValidationError String
validateName s
  | null s = Left (FieldEmpty "name")
  | otherwise = Right s

-- Complete validation pipeline
validateRegistration :: String -> String -> String -> Int -> Either ValidationError User
validateRegistration name email password age = do
  validName <- validateName name
  validEmail <- validateEmail email
  validPassword <- validatePassword password
  validAge <- validateAge age
  
  return $ User validName validEmail validPassword validAge

-- Usage
main :: IO ()
main = do
  -- Valid registration
  print $ validateRegistration "Alice" "alice@example.com" "password123" 25
  -- Right (User "Alice" (Email "alice@example.com") (Password "password123") 25)
  
  -- Invalid email
  print $ validateRegistration "Bob" "not-an-email" "password123" 30
  -- Left (EmailInvalid "not-an-email")
  
  -- Password too short
  print $ validateRegistration "Charlie" "charlie@example.com" "pass" 30
  -- Left (PasswordTooShort 4)
  
  -- Too young
  print $ validateRegistration "David" "david@example.com" "password123" 16
  -- Left (AgeTooYoung 16)
  
  -- Multiple errors (fails fast at first error)
  print $ validateRegistration "" "bad-email" "pass" 10
  -- Left (FieldEmpty "name")

-- Railway-oriented: collect ALL errors instead of failing fast
data ValidationResult e a = Failure [e] | Success a

validateRegistrationAll :: String -> String -> String -> Int -> ValidationResult ValidationError User
validateRegistrationAll name email password age =
  case (validateName name, validateEmail email, validatePassword password, validateAge age) of
    (Right n, Right e, Right p, Right a) -> Success (User n e p a)
    results -> Failure (concatMap extractErrors [results])
  where
    extractErrors (Left e) = [e]
    extractErrors _ = []

-- Usage
mainAll :: IO ()
mainAll = do
  print $ validateRegistrationAll "" "bad-email" "pass" 10
  -- Failure [FieldEmpty "name", EmailInvalid "bad-email", PasswordTooShort 4, AgeTooYoung 10]
```

---

## Example 3: Immutable Data Structures with Lenses

### Scenario: Updating Nested User Data

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Control.Lens.TH

-- Nested data structure
data Address = Address
  { _addressStreet :: String
  , _addressCity :: String
  , _addressZip :: String
  , _addressCountry :: String
  } deriving (Show)

data UserProfile = UserProfile
  { _profilePhone :: Maybe String
  , _profileWebsite :: Maybe String
  , _profileBio :: String
  } deriving (Show)

data User = User
  { _userId :: Int
  , _userName :: String
  , _userEmail :: String
  , _userAddress :: Address
  , _userProfile :: UserProfile
  } deriving (Show)

-- Generate lenses
makeLenses ''Address
makeLenses ''UserProfile
makeLenses ''User

-- Sample data
john :: User
john = User
  { _userId = 1
  , _userName = "John Doe"
  , _userEmail = "john@example.com"
  , _userAddress = Address
      { _addressStreet = "123 Main St"
      , _addressCity = "New York"
      , _addressZip = "10001"
      , _addressCountry = "USA"
      }
  , _userProfile = UserProfile
      { _profilePhone = Just "555-1234"
      , _profileWebsite = Just "https://johndoe.com"
      , _profileBio = "Software developer"
      }
  }

-- Update operations using lenses
main :: IO ()
main = do
  -- Update city (deep nested update)
  let johnInBoston = john & userAddress . addressCity .~ "Boston"
  print $ johnInBoston ^. userAddress . addressCity  -- "Boston"
  
  -- Original unchanged
  print $ john ^. userAddress . addressCity  -- "New York"
  
  -- Update multiple fields
  let movedJohn = john
        & userAddress . addressCity .~ "San Francisco"
        & userAddress . addressStreet .~ "456 Market St"
        & userAddress . addressZip .~ "94102"
        & userName .~ "John Smith"
  print $ movedJohn ^. userAddress . addressCity  -- "San Francisco"
  
  -- Safe update of Maybe fields
  let johnWithNewPhone = john & userProfile . profilePhone . _Just .~ "555-5678"
  print $ johnWithNewPhone ^. userProfile . profilePhone  -- Just "555-5678"
  
  -- Safe access (returns Maybe)
  let phone :: Maybe String
      phone = john ^? userProfile . profilePhone . _Just
  print phone  -- Just "555-1234"
  
  -- Update inside Maybe (if Nothing, stays Nothing)
  let johnNoPhone = john & userProfile . profilePhone .~ Nothing
  let johnWithPhone = johnNoPhone & userProfile . profilePhone . _Just .~ "555-9999"
  print $ johnWithPhone ^. userProfile . profilePhone  -- Nothing (no change)
  
  -- Compose multiple operations
  let fullyUpdated = john
        & userEmail .~ "john.smith@example.com"
        & userAddress . addressCity .~ "Seattle"
        & userAddress . addressStreet .~ "789 Pine St"
        & userProfile . profileBio .~ "Senior software architect"
        & userProfile . profileWebsite . _Just .~ "https://johnsmith.dev"
  print $ fullyUpdated ^. userEmail  -- "john.smith@example.com"
```

---

## Example 4: Parser Combinators for DSL

### Scenario: Parsing a Simple Configuration Language

```haskell
{-# LANGUAGE LambdaCase #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

-- AST for configuration
data Config = Config
  { configName :: String
  , configPort :: Int
  , configDebug :: Bool
  , configDatabase :: DatabaseConfig
  } deriving (Show)

data DatabaseConfig = DatabaseConfig
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  } deriving (Show)

-- Parser type
type Parser = Parsec Void Text

-- Lexer helpers
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"') <* spaceConsumer

integer :: Parser Int
integer = lexeme L.decimal

-- Parsers
configParser :: Parser Config
configParser = between spaceConsumer eof $ do
  name <- section "server" serverParser
  db <- section "database" databaseParser
  return $ Config name 8080 False db  -- Default values

serverParser :: Parser String
serverParser = do
  symbol "name"
  char '='
  spaceConsumer
  name <- stringLiteral
  return name

databaseParser :: Parser DatabaseConfig
databaseParser = do
  symbol "host"
  char '='
  spaceConsumer
  host <- stringLiteral
  
  symbol "port"
  char '='
  spaceConsumer
  port <- integer
  
  symbol "name"
  char '='
  spaceConsumer
  name <- stringLiteral
  
  return $ DatabaseConfig host port name

section :: String -> Parser a -> Parser a
section name p = do
  symbol (T.pack $ "[" ++ name ++ "]")
  result <- p
  return result

-- Example configuration
sampleConfig :: Text
sampleConfig = T.pack $ unlines
  [ "[server]"
  , "name = \"myapp\""
  , ""
  , "[database]"
  , "host = \"localhost\""
  , "port = 5432"
  , "name = \"mydb\""
  ]

-- Usage
main :: IO ()
main = do
  case parse configParser "config.conf" sampleConfig of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right config -> print config
  -- Config {configName = "myapp", configPort = 8080, configDebug = False, 
  --         configDatabase = DatabaseConfig {dbHost = "localhost", dbPort = 5432, dbName = "mydb"}}

-- More complex: Expression parser
data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Show)

exprParser :: Parser Expr
exprParser = makeExprParser term operators
  where
    term = parens exprParser
       <|> Num <$> integer
       <|> Var <$> identifier
    
    identifier :: Parser String
    identifier = lexeme $ some letterChar
    
    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")
    
    operators :: [[Operator Parser Expr]]
    operators =
      [ [ InfixL (Mul <$ symbol "*") ]
      , [ InfixL (Add <$ symbol "+") ]
      ]

-- Test expression parsing
testExpr :: Text
testExpr = "3 + 4 * (2 + 1)"

mainExpr :: IO ()
mainExpr = do
  case parse exprParser "expr" testExpr of
    Left err -> print err
    Right expr -> print expr  -- Add (Num 3) (Mul (Num 4) (Add (Num 2) (Num 1)))
```

---

## Example 5: Type-Safe Web API with Servant

### Scenario: Building a REST API with Compile-Time Safety

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

import Servant
import Servant.Server
import Network.Wai.Handler.Warp (run)
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Domain model
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

type Users = Map.Map Int User

-- API specification (at type level!)
type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "userId" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int
  :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] User
  :<|> "users" :> Capture "userId" Int :> Delete '[JSON] ()

-- Server implementation
server :: TVar Users -> Server UserAPI
server usersVar = 
       getUsers usersVar
  :<|> getUser usersVar
  :<|> createUser usersVar
  :<|> updateUser usersVar
  :<|> deleteUser usersVar

getUsers :: TVar Users -> Handler [User]
getUsers usersVar = do
  users <- liftIO $ atomically $ readTVar usersVar
  return $ Map.elems users

getUser :: TVar Users -> Int -> Handler User
getUser usersVar userId = do
  users <- liftIO $ atomically $ readTVar usersVar
  case Map.lookup userId users of
    Nothing -> throwError err404 { errBody = "User not found" }
    Just user -> return user

createUser :: TVar Users -> User -> Handler Int
createUser usersVar user = do
  users <- liftIO $ atomically $ readTVar usersVar
  let newId = maybe 0 ((+1) . fst) (Map.lookupMax users)
      newUser = user { userId = newId }
  liftIO $ atomically $ modifyTVar usersVar (Map.insert newId newUser)
  return newId

updateUser :: TVar Users -> Int -> User -> Handler User
updateUser usersVar userId user = do
  users <- liftIO $ atomically $ readTVar usersVar
  if Map.member userId users
    then do
      let updatedUser = user { userId = userId }
      liftIO $ atomically $ modifyTVar usersVar (Map.insert userId updatedUser)
      return updatedUser
    else throwError err404 { errBody = "User not found" }

deleteUser :: TVar Users -> Int -> Handler ()
deleteUser usersVar userId = do
  users <- liftIO $ atomically $ readTVar usersVar
  if Map.member userId users
    then liftIO $ atomically $ modifyTVar usersVar (Map.delete userId)
    else throwError err404 { errBody = "User not found" }

-- API Proxy
userAPI :: Proxy UserAPI
userAPI = Proxy

-- Application
app :: TVar Users -> Application
app usersVar = serve userAPI (server usersVar)

main :: IO ()
main = do
  -- Initialize empty user database
  usersVar <- atomically $ newTVar Map.empty
  
  -- Seed with sample data
  atomically $ modifyTVar usersVar $ Map.insert 1 $ User 1 "Alice" "alice@example.com"
  atomically $ modifyTVar usersVar $ Map.insert 2 $ User 2 "Bob" "bob@example.com"
  
  putStrLn "Server running on port 8080"
  run 8080 (app usersVar)

-- Client functions (automatically generated from API type!)
getUsersClient :: ClientM [User]
getUserClient :: Int -> ClientM User
createUserClient :: User -> ClientM Int
updateUserClient :: Int -> User -> ClientM User
deleteUserClient :: Int -> ClientM ()

(getUsersClient :<|> getUserClient :<|> createUserClient :<|> updateUserClient :<|> deleteUserClient) = 
  client userAPI

-- Example client usage
exampleClient :: ClientM ()
exampleClient = do
  -- Get all users
  users <- getUsersClient
  liftIO $ print users
  
  -- Get specific user
  user <- getUserClient 1
  liftIO $ print user
  
  -- Create new user
  newUserId <- createUserClient $ User 0 "Charlie" "charlie@example.com"
  liftIO $ print newUserId
  
  -- Update user
  updated <- updateUserClient 1 $ User 1 "Alice Smith" "alice.smith@example.com"
  liftIO $ print updated
  
  -- Delete user
  deleteUserClient 2
```

---

## Example 6: State Management with Monad Transformers

### Scenario: Game State Management

```haskell
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.Random

-- Game domain
data GameState = GameState
  { gsPlayerPos :: (Int, Int)
  , gsScore :: Int
  , gsInventory :: [Item]
  , gsHealth :: Int
  } deriving (Show)

data Item = Sword | Potion | Key | Treasure deriving (Show, Eq)

data GameConfig = GameConfig
  { gcMaxHealth :: Int
  , gcStartPos :: (Int, Int)
  } deriving (Show)

data GameError = GameOver String | InvalidMove String deriving (Show)

-- Game monad stack
type Game = StateT GameState (ReaderT GameConfig (Except GameError)) ()

-- Game functions
movePlayer :: (Int, Int) -> Game
movePlayer (dx, dy) = do
  (x, y) <- gets gsPlayerPos
  let newPos = (x + dx, y + dy)
  modify $ \s -> s { gsPlayerPos = newPos }

collectItem :: Item -> Game
collectItem item = modify $ \s -> s { gsInventory = item : gsInventory s }

takeDamage :: Int -> Game
takeDamage dmg = do
  currentHealth <- gets gsHealth
  maxHealth <- asks gcMaxHealth
  
  let newHealth = max 0 (currentHealth - dmg)
  modify $ \s -> s { gsHealth = newHealth }
  
  when (newHealth == 0) $
    throwError $ GameOver "You died!"

addScore :: Int -> Game
addScore points = modify $ \s -> s { gsScore = gsScore s + points }

usePotion :: Game
usePotion = do
  inventory <- gets gsInventory
  if Potion `elem` inventory
    then do
      modify $ \s -> s { gsInventory = filter (/= Potion) (gsInventory s) }
      modify $ \s -> s { gsHealth = gcMaxHealth (undefined) }  -- Simplified
      maxHealth <- asks gcMaxHealth
      modify $ \s -> s { gsHealth = maxHealth }
    else
      throwError $ InvalidMove "No potion in inventory"

-- Complex game action
findTreasure :: Game
findTreasure = do
  -- Move to treasure location
  movePlayer (5, 3)
  
  -- Add treasure to inventory
  collectItem Treasure
  
  -- Add score
  addScore 1000
  
  -- Check for trap (random damage)
  gen <- liftIO newStdGen
  let (trap, _) = randomR (0, 1) gen :: (Int, StdGen)
  when (trap == 1) $ do
    liftIO $ putStrLn "You triggered a trap!"
    takeDamage 20

-- Run game
initialState :: GameState
initialState = GameState
  { gsPlayerPos = (0, 0)
  , gsScore = 0
  , gsInventory = [Sword, Potion]
  , gsHealth = 100
  }

config :: GameConfig
config = GameConfig
  { gcMaxHealth = 100
  , gcStartPos = (0, 0)
  }

runGame :: Game -> Either GameError GameState
runGame game = runExcept $ runReaderT (execStateT game initialState) config

main :: IO ()
main = do
  case runGame findTreasure of
    Left err -> print err
    Right finalState -> print finalState
```

---

## Example 7: Property-Based Testing

### Scenario: Testing Invariants with QuickCheck

```haskell
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List (sort, nub)

-- Property: reverse is involutive (reverse . reverse = id)
prop_reverseInvolutive :: [Int] -> Bool
prop_reverseInvolutive xs = reverse (reverse xs) == xs

-- Property: sort is idempotent (sort . sort = sort)
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs

-- Property: addition is commutative
prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

-- Property: list length is preserved by map
prop_mapLength :: Fun Int Int -> [Int] -> Bool
prop_mapLength (Fn f) xs = length (map f xs) == length xs

-- Property: filter length is less than or equal to original
prop_filterLength :: (Int -> Bool) -> [Int] -> Bool
prop_filterLength p xs = length (filter p xs) <= length xs

-- Property: reverse preserves length
prop_reverseLength :: [Int] -> Bool
prop_reverseLength xs = length (reverse xs) == length xs

-- Property: sort produces ordered list
prop_sortOrdered :: [Int] -> Bool
prop_sortOrdered xs = isSorted (sort xs)
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Property: nub removes duplicates
prop_nubNoDuplicates :: [Int] -> Bool
prop_nubNoDuplicates xs = length (nub xs) == length (nub xs)  -- Trivial, but check no duplicates
  && allUnique (nub xs)
  where
    allUnique [] = True
    allUnique (x:xs) = x `notElem` xs && allUnique xs

-- Property: distributive law
prop_distributive :: Int -> Int -> Int -> Bool
prop_distributive x y z = x * (y + z) == x * y + x * z

-- Property: monad law - left identity
prop_monadLeftIdentity :: Int -> Fun Int (Maybe Int) -> Bool
prop_monadLeftIdentity x (Fn f) = (return x >>= f) == f x

-- Property: monad law - right identity
prop_monadRightIdentity :: Maybe Int -> Bool
prop_monadRightIdentity mx = (mx >>= return) == mx

-- Property: monad law - associativity
prop_monadAssociativity :: Maybe Int -> Fun Int (Maybe Int) -> Fun Int (Maybe Int) -> Bool
prop_monadAssociativity mx (Fn f) (Fn g) = 
  ((mx >>= f) >>= g) == (mx >>= (\x -> f x >>= g))

-- Custom data type
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where
      tree' 0 = Leaf <$> arbitrary
      tree' n | n > 0 = oneof
        [ Leaf <$> arbitrary
        , Node <$> tree' (n `div` 2) <*> tree' (n `div` 2)
        ]

-- Property: map preserves structure
prop_mapStructure :: Fun Int Int -> Tree Int -> Bool
prop_mapStructure (Fn f) tree = collect (size tree) $ 
  structure (mapTree f tree) == structure tree
  where
    mapTree f (Leaf x) = Leaf (f x)
    mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)
    
    structure (Leaf _) = "Leaf"
    structure (Node l r) = "Node " ++ structure l ++ " " ++ structure r
    
    size (Leaf _) = 1
    size (Node l r) = size l + size r

-- Run all tests
main :: IO ()
main = do
  putStrLn "Testing reverse involutive..."
  quickCheck prop_reverseInvolutive
  
  putStrLn "Testing sort idempotent..."
  quickCheck prop_sortIdempotent
  
  putStrLn "Testing addition commutative..."
  quickCheck prop_addCommutative
  
  putStrLn "Testing map length preservation..."
  quickCheck prop_mapLength
  
  putStrLn "Testing sort ordered..."
  quickCheck prop_sortOrdered
  
  putStrLn "Testing monad laws..."
  quickCheck prop_monadLeftIdentity
  quickCheck prop_monadRightIdentity
  quickCheck prop_monadAssociativity
  
  putStrLn "Testing tree map structure..."
  quickCheck prop_mapStructure
```

---

## Example 8: Concurrent Programming with STM

### Scenario: Bank Account Transfers

```haskell
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

-- Account type
type Account = TVar Double

-- Create account
createAccount :: Double -> STM Account
createAccount balance = newTVar balance

-- Get balance
getBalance :: Account -> STM Double
getBalance = readTVar

-- Deposit money
deposit :: Account -> Double -> STM ()
deposit account amount = do
  current <- readTVar account
  writeTVar account (current + amount)

-- Withdraw money (with validation)
withdraw :: Account -> Double -> STM Bool
withdraw account amount = do
  current <- readTVar account
  if current >= amount
    then do
      writeTVar account (current - amount)
      return True
    else return False

-- Transfer between accounts
transfer :: Account -> Account -> Double -> STM Bool
transfer from to amount = do
  success <- withdraw from amount
  when success $ deposit to amount
  return success

-- Concurrent transfers
concurrentTransfers :: IO ()
concurrentTransfers = do
  -- Create accounts
  account1 <- atomically $ createAccount 1000
  account2 <- atomically $ createAccount 500
  
  -- Print initial balances
  bal1 <- atomically $ getBalance account1
  bal2 <- atomically $ getBalance account2
  putStrLn $ "Initial: Account1 = " ++ show bal1 ++ ", Account2 = " ++ show bal2
  
  -- Start multiple transfer threads
  let numThreads = 10
  threads <- forM [1..numThreads] $ \i -> do
    forkIO $ do
      -- Random delay
      delay <- randomRIO (1, 100)
      threadDelay (delay * 1000)
      
      -- Random amount
      amount <- randomRIO (10, 100)
      
      -- Random direction
      direction <- randomRIO (0, 1) :: IO Int
      
      success <- atomically $ if direction == 0
        then transfer account1 account2 amount
        else transfer account2 account1 amount
      
      putStrLn $ "Thread " ++ show i ++ ": Transfer " ++ show amount ++ 
                 " " ++ (if direction == 0 then "1->2" else "2->1") ++
                 " " ++ (if success then "SUCCESS" else "FAILED")
  
  -- Wait for all threads
  mapM_ takeMVar =<< forM threads (\_ -> newEmptyMVar)
  
  -- Print final balances
  bal1' <- atomically $ getBalance account1
  bal2' <- atomically $ getBalance account2
  putStrLn $ "Final: Account1 = " ++ show bal1' ++ ", Account2 = " ++ show bal2'
  putStrLn $ "Total = " ++ show (bal1' + bal2')  -- Should be 1500

-- Composable transactions
composedTransaction :: Account -> Account -> Account -> Double -> Double -> STM Bool
composedTransaction acc1 acc2 acc3 amount1 amount2 = do
  -- Transfer from acc1 to acc2
  success1 <- transfer acc1 acc2 amount1
  
  -- Transfer from acc2 to acc3
  success2 <- if success1 then transfer acc2 acc3 amount2 else return False
  
  return (success1 && success2)

main :: IO ()
main = do
  putStrLn "Running concurrent transfers..."
  concurrentTransfers
```

---

## Summary

These examples demonstrate core functional programming patterns in Haskell:

1. **Error Handling**: Use `Either` and `Maybe` for type-safe error handling instead of exceptions
2. **Monadic Composition**: Chain operations with `do` notation or `>>=`
3. **Immutable Data**: Use lenses for deep updates to nested records
4. **Parser Combinators**: Build parsers by composing small parsers
5. **Type Safety**: Leverage Servant for compile-time API verification
6. **State Management**: Use monad transformers for composable effects
7. **Testing**: Use property-based testing to verify invariants
8. **Concurrency**: Use STM for composable, atomic transactions

**Key Principle**: Compose small, pure functions into larger computations. Let the type system guide you and catch errors at compile-time.

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global Rules System  
**Status**: Active