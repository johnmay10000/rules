---
title: Haskell Functional Programming Libraries Guide
language: haskell
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Haskell Functional Programming Libraries Guide

This guide covers the essential libraries for functional programming in Haskell, their use cases, and best practices.

---

## Core FP Libraries

### 1. `mtl` - Monad Transformer Library

**Purpose**: Provides standard monad transformers (Reader, Writer, State, Except) and type classes for composing effects.

**Installation**:
```yaml
# In package.yaml or .cabal file
dependencies:
  - mtl >= 2.3
```

**Key Modules**:
- `Control.Monad.Reader` - Reader monad for dependency injection
- `Control.Monad.State` - State monad for stateful computations
- `Control.Monad.Writer` - Writer monad for logging/accumulation
- `Control.Monad.Except` - Except monad for error handling
- `Control.Monad.RWS` - Combined Reader/Writer/State

**When to Use**:
- When you need to compose multiple effects (reading config, state, errors)
- For dependency injection without explicit parameters
- For stateful computations that should remain pure

**Example**:
```haskell
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data Config = Config { dbConnection :: String, apiKey :: String }
data AppError = NotFound | PermissionDenied

-- Combine effects
type App = ReaderT Config (StateT AppState (Except AppError)) 

data AppState = AppState { requestCount :: Int }

getUser :: UserId -> App User
getUser userId = do
  config <- ask  -- From ReaderT
  lift $ modify (\s -> s { requestCount = requestCount s + 1 })  -- From StateT
  result <- liftIO $ queryDB (dbConnection config) userId
  case result of
    Nothing -> throwError NotFound  -- From ExceptT
    Just user -> return user

-- Run it
runApp :: App a -> Config -> AppState -> Either AppError (a, AppState)
runApp app config initialState = 
  runExcept $ runStateT (runReaderT app config) initialState
```

**Why It's Essential**: The standard way to compose effects in Haskell. Industry standard.

---

### 2. `lens` - Functional References

**Purpose**: Provides lenses, prisms, and traversals for immutable updates to nested data structures.

**Installation**:
```yaml
dependencies:
  - lens >= 5.2
```

**Key Concepts**:
- `Lens` - Focus on a field in a record
- `Prism` - Focus on a constructor in a sum type
- `Traversal` - Focus on multiple values
- `Iso` - Isomorphism between types

**When to Use**:
- For deep updates to nested records
- For accessing fields in nested data structures
- For working with large, complex data models

**Example**:
```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

-- Define data
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

-- Deep update (immutable)
updatedUser :: User
updatedUser = user & userAddress . addressCity .~ "Boston"

-- View nested field
city :: String
city = user ^. userAddress . addressCity  -- "NYC"

-- Compose multiple updates
movedUser :: User
movedUser = user
  & userAddress . addressCity .~ "Los Angeles"
  & userAddress . addressStreet .~ "456 Hollywood Blvd"
  & userName .~ "Alice Smith"

-- Safe access to Maybe fields
data UserProfile = UserProfile
  { _profilePhone :: Maybe String
  , _profileWebsite :: Maybe String
  } deriving (Show)

makeLenses ''UserProfile

profile :: UserProfile
profile = UserProfile (Just "555-1234") Nothing

-- Update inside Maybe
updatedProfile :: UserProfile
updatedProfile = profile & profilePhone . _Just .~ "555-5678"

-- Safe access
phone :: Maybe String
phone = profile ^? profilePhone . _Just  -- Just "555-1234"
```

**Why It's Essential**: Makes immutable updates to nested structures practical. Used in production systems.

---

### 3. `aeson` - JSON Handling

**Purpose**: JSON encoding and decoding with type safety.

**Installation**:
```yaml
dependencies:
  - aeson >= 2.1
```

**Key Functions**:
- `encode` - Convert value to JSON ByteString
- `decode` - Parse JSON to Maybe value
- `eitherDecode` - Parse JSON to Either Error value
- `FromJSON` / `ToJSON` type classes

**When to Use**:
- For JSON APIs
- For configuration files
- For data interchange

**Example**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics

-- Define types
data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Show, Generic)

-- Derive JSON instances automatically
instance FromJSON User
instance ToJSON User

-- Usage
user :: User
user = User 1 "Alice" "alice@example.com"

-- Encode to JSON
json :: ByteString
json = encode user
-- {"userId":1,"userName":"Alice","userEmail":"alice@example.com"}

-- Decode from JSON
decoded :: Maybe User
decoded = decode json  -- Just (User 1 "Alice" "alice@example.com")

-- Type-safe parsing
parseUser :: ByteString -> Either String User
parseUser = eitherDecode

-- Custom instances
data UserV2 = UserV2
  { userV2Id :: Int
  , userV2Name :: String
  , userV2Email :: String
  , userV2CreatedAt :: UTCTime
  } deriving (Show)

instance FromJSON UserV2 where
  parseJSON = withObject "UserV2" $ \v -> UserV2
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "email"
    <*> v .: "created_at"

instance ToJSON UserV2 where
  toJSON u = object
    [ "id" .= userV2Id u
    , "name" .= userV2Name u
    , "email" .= userV2Email u
    , "created_at" .= userV2CreatedAt u
    ]
```

**Why It's Essential**: Type-safe JSON handling. Automatic derivation reduces boilerplate.

---

### 4. `servant` - Type-Safe Web APIs

**Purpose**: Type-level specification of web APIs with automatic client/server generation.

**Installation**:
```yaml
dependencies:
  - servant-server >= 0.20
  - servant-client >= 0.20
```

**Key Features**:
- Type-safe API specification
- Automatic server implementation
- Automatic client generation
- Automatic documentation generation

**When to Use**:
- For REST APIs
- When you want compile-time guarantee that client and server match
- For microservices

**Example**:
```haskell
{-# LANGUAGE DataKinds, TypeOperators #-}
import Servant

-- Type-level API specification
type UserAPI = 
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "userId" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int
  :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] User

-- Server implementation
server :: Server UserAPI
server = getUsers :<|> getUser :<|> createUser :<|> updateUser

getUsers :: Handler [User]
getUsers = do
  users <- liftIO $ queryDB "SELECT * FROM users"
  return users

getUser :: Int -> Handler User
getUser userId = do
  maybeUser <- liftIO $ queryDB $ "SELECT * FROM users WHERE id = " ++ show userId
  case maybeUser of
    Nothing -> throwError err404
    Just user -> return user

createUser :: User -> Handler Int
createUser user = liftIO $ insertDB user

updateUser :: Int -> User -> Handler User
updateUser userId user = liftIO $ updateDB userId user

-- Run server
app :: Application
app = serve (Proxy :: Proxy UserAPI) server

main :: IO ()
main = run 8080 app

-- Automatic client generation (type-safe!)
getUsersClient :: ClientM [User]
getUserClient :: Int -> ClientM User
createUserClient :: User -> ClientM Int
updateUserClient :: Int -> User -> ClientM User

(getUsersClient :<|> getUserClient :<|> createUserClient :<|> updateUserClient) = 
  client (Proxy :: Proxy UserAPI)
```

**Why It's Essential**: Guarantees at compile-time that your API client and server match. Eliminates whole classes of bugs.

---

### 5. `persistent` - Type-Safe Database ORM

**Purpose**: Type-safe database operations with compile-time checked queries.

**Installation**:
```yaml
dependencies:
  - persistent >= 2.14
  - persistent-sqlite >= 2.13
  - persistent-template >= 2.12
```

**Key Features**:
- Type-safe queries
- Compile-time migration checking
- Automatic model generation
- Multiple backend support (SQLite, PostgreSQL, MySQL, MongoDB)

**When to Use**:
- For database-backed applications
- When you want compile-time query checking
- For type-safe migrations

**Example**:
```haskell
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

-- Define models (generates types and instances)
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name String
  email String
  age Int Maybe
  deriving Show
|]

-- Type-safe queries
getAdults :: SqlPersistM [Entity User]
getAdults = selectList [UserAge >=. Just 18] []

getUserByEmail :: String -> SqlPersistM (Maybe (Entity User))
getUserByEmail email = selectFirst [UserEmail ==. email] []

-- Insert
createUser :: String -> String -> Maybe Int -> SqlPersistM UserId
createUser name email age = insert $ User name email age

-- Update
updateUserAge :: UserId -> Int -> SqlPersistM ()
updateUserAge userId newAge = update userId [UserAge =. Just newAge]

-- Transactions
transfer :: UserId -> UserId -> Int -> SqlPersistM ()
transfer from to amount = do
  fromUser <- get from
  toUser <- get to
  case (fromUser, toUser) of
    (Just (User _ _ (Just fromBalance)), Just (User _ _ (Just toBalance))) -> do
      update from [UserAge =. Just (fromBalance - amount)]
      update to [UserAge =. Just (toBalance + amount)]
    _ -> error "Invalid user or balance"
```

**Why It's Essential**: Type-safe database operations catch errors at compile-time. Automatic migrations reduce boilerplate.

---

## Testing Libraries

### 6. `QuickCheck` - Property-Based Testing

**Purpose**: Automatic generation of test cases based on properties.

**Installation**:
```yaml
dependencies:
  - QuickCheck >= 2.14
```

**Key Features**:
- Automatic test case generation
- Shrinking (finds minimal failing case)
- Custom generators
- Typeclass law testing

**When to Use**:
- For testing invariants and properties
- For finding edge cases
- For testing typeclass laws

**Example**:
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
  where
    divide _ 0 = Left "divide by zero"
+    divide x y = Right (x / y)

-- Custom generators
+genUser :: Gen User
+genUser = User <$> arbitrary <*> arbitrary <*> arbitrary

+-- Run tests
+main :: IO ()
+main = do
+  quickCheck prop_reverseInvolutive
+  quickCheck prop_sortIdempotent
+  quickCheck prop_addCommutative
+  quickCheck prop_divideBySelf
```

**Why It's Essential**: Finds bugs you wouldn't think to test for. Shrinking gives you minimal failing cases.

---

### 7. `hedgehog` - Property-Based Testing with Better Shrinking

**Purpose**: Alternative to QuickCheck with integrated shrinking and better error messages.

**Installation**:
```yaml
dependencies:
  - hedgehog >= 1.2
```

**Key Features**:
- Integrated shrinking (no separate implementation needed)
- Better error messages
- Monad-based generators
- Property testing with state machines

**When to Use**:
- When you want better shrinking than QuickCheck
- For complex state machine testing
- For clearer error messages

**Example**:
```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Property
+prop_reverse :: Property
+prop_reverse = property $ do
+  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
+  reverse (reverse xs) === xs

+-- Custom generator
+genUser :: Gen User
+genUser = User 
+  <$> Gen.int (Range.linear 1 1000)
+  <*> Gen.string (Range.linear 3 20) Gen.alpha
+  <*> Gen.string (Range.linear 5 30) Gen.alphaNum

+-- State machine testing
+prop_stateMachine :: Property
+prop_stateMachine = property $ do
+  actions <- forAll $ Gen.list (Range.linear 0 50) genAction
+  initialState <- forAll genInitialState
+  executeActions initialState actions === valid
```

**Why It's Essential**: Better shrinking and error messages than QuickCheck. Monad-based generators are more composable.

---

## Utility Libraries

### 8. `text` and `bytestring` - Efficient Strings

**Purpose**: High-performance string types.

**Installation**:
```yaml
dependencies:
  - text >= 2.0
  - bytestring >= 0.11
```

**Key Types**:
- `Text` - Unicode strings
- `ByteString` - Binary data

**When to Use**:
- `Text` for all string processing (not `String`)
- `ByteString` for binary data, network protocols

**Example**:
```haskell
import Data.Text (Text)
+import qualified Data.Text as T
+import Data.ByteString (ByteString)
+import qualified Data.ByteString as BS

+-- Text operations (efficient)
+greeting :: Text
+greeting = T.pack "Hello, World!"
+
+upper :: Text
+upper = T.toUpper greeting  -- "HELLO, WORLD!"
+
+-- ByteString for binary
+packet :: ByteString
+packet = BS.pack [0x01, 0x02, 0x03, 0x04]
+
+-- Efficient concatenation
+concatTexts :: [Text] -> Text
+concatTexts = T.concat
+
+-- Lazy vs Strict
+import Data.Text.Lazy (LTxt)
+import qualified Data.Text.Lazy as LT
+
+lazyText :: LText
+lazyText = LT.pack "Large text that can be streamed"
```

**Why It's Essential**: `String` is inefficient. Use `Text` for production code.

---

### 9. `containers` - Efficient Data Structures

**Purpose**: High-performance containers (Map, Set, IntMap, IntSet, Sequence).

**Installation**:
```yaml
dependencies:
  - containers >= 0.6
```

**Key Modules**:
- `Data.Map` - Key-value maps
- `Data.Set` - Unique value sets
- `Data.IntMap` - Maps with Int keys (faster)
- `Data.Sequence` - Efficient sequences

**When to Use**:
- `Map` for key-value storage
- `Set` for unique collections
- `Sequence` for efficient append/prepand

**Example**:
```haskell
import qualified Data.Map as Map
+import qualified Data.Set as Set
+import qualified Data.Sequence as Seq

+-- Map operations
+userAges :: Map.Map String Int
+userAges = Map.fromList [("Alice", 30), ("Bob", 25)]
+
+lookupAge :: String -> Maybe Int
+lookupAge name = Map.lookup name userAges
+
+insertAge :: String -> Int -> Map.Map String Int
+insertAge name age = Map.insert name age userAges
+
+-- Set operations
+activeUsers :: Set.Set String
+activeUsers = Set.fromList ["Alice", "Bob", "Charlie"]
+
+isActive :: String -> Bool
+isActive user = Set.member user activeUsers
+
+-- Sequence operations (efficient append/prepand)
+queue :: Seq.Seq Int
+queue = Seq.empty Seq.|> 1 Seq.|> 2 Seq.|> 3
+
+dequeued :: Maybe (Int, Seq.Seq Int)
+dequeued = case Seq.viewl queue of
+  Seq.EmptyL -> Nothing
+  x Seq.:< xs -> Just (x, xs)
```

**Why It's Essential**: More efficient than lists for many operations. Type-safe collections.

---

### 10. `vector` - Efficient Arrays

**Purpose**: High-performance, type-safe arrays.

**Installation**:
```yaml
dependencies:
  - vector >= 0.13
```

**Key Features**:
- O(1) indexing
- Efficient slicing
- Fusion optimization
- Multiple variants (boxed, unboxed, storable)

**When to Use**:
- For numerical computations
- For large datasets requiring random access
- When performance is critical

**Example**:
```haskell
import Data.Vector (Vector)
+import qualified Data.Vector as V
+
+-- Create vectors
+vec1 :: Vector Int
+vec1 = V.fromList [1,2,3,4,5]
+
+vec2 :: Vector Int
+vec2 = V.enumFromN 1 5  -- [1,2,3,4,5]
+
+-- Efficient operations
+sumVec :: Int
+sumVec = V.sum vec1  -- 15
+
+mapped :: Vector Int
+mapped = V.map (*2) vec1  -- [2,4,6,8,10]
+
+-- Indexing
+third :: Int
+third = vec1 V.! 2  -- 3 (0-indexed)
+
+-- Slicing
+middle :: Vector Int
+middle = V.slice 1 3 vec1  -- [2,3,4]
+
+-- Fusion (no intermediate vectors)
+fused :: Int
+fused = V.sum $ V.map (*2) $ V.filter even vec1  -- 12
+-- No intermediate vectors created!
```

**Why It's Essential**: Much more efficient than lists for numeric operations and random access.

---

## Library Integration Patterns

### Combining mtl and lens

```haskell
-- App monad with state that has nested records
type App = StateT AppState (ReaderT Config IO)

data AppState = AppState
  { _appUser :: User
  , _appSession :: Session
  } deriving (Show)

makeLenses ''AppState

-- Update nested state in monad
+updateUserEmail :: String -> App ()
+updateUserEmail newEmail = do
+  appUser . userEmail .= newEmail
+
+getUserName :: App String
+getUserName = use (appUser . userName)
```

### Combining aeson and persistent

```haskell
-- Derive both JSON and Persist instances
+share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
+User
+  name String
+  email String
+  deriving Show Generic
+|]
+
+instance FromJSON User
+instance ToJSON User
+
+-- Now you can:
+-- 1. Store in database
+userId <- insert user
+
+-- 2. Serialize to JSON
+let json = encode user
+
+-- 3. Both seamlessly
+getUserJSON :: UserId -> App ByteString
+getUserJSON userId = do
+  maybeUser <- get userId
+  case maybeUser of
+    Nothing -> throwError NotFound
+    Just user -> return $ encode user
```

---

## Library Selection Guide

| Use Case | Primary Library | Alternatives | Notes |
+|----------|----------------|--------------|-------|
+| Effect composition | `mtl` | `fused-effects`, `polysemy` | mtl is standard |
+| Immutable updates | `lens` | `optics`, `microlens` | lens is most powerful |
+| JSON | `aeson` | `json` (simpler) | aeson is standard |
+| Web APIs | `servant` | `scotty`, `spock` | servant is type-safe |
+| Database | `persistent` | `beam`, `opaleye` | persistent is simpler |
+| Testing | `QuickCheck` | `hedgehog` | QuickCheck is standard |
+| Property testing | `hedgehog` | `QuickCheck` | hedgehog has better shrinking |
+| Strings | `text` | - | Always use instead of String |
+| Collections | `containers` | `unordered-containers` | containers is standard |
+| Arrays | `vector` | `array` | vector is more modern |

---

## Version Compatibility

```yaml
# Recommended versions for GHC 9.6+
resolver: lts-21.17

dependencies:
+  - base >= 4.18 && < 5
+  - mtl >= 2.3
+  - lens >= 5.2
+  - aeson >= 2.1
+  - servant-server >= 0.20
+  - persistent >= 2.14
+  - QuickCheck >= 2.14
+  - hedgehog >= 1.2
+  - text >= 2.0
+  - containers >= 0.6
+  - vector >= 0.13
```

---

## Further Reading

- **mtl**: [Haskell Wiki - Monad Transformers](https://wiki.haskell.org/Monad_Transformers)
- **lens**: [Lens Tutorial](https://hackage.haskell.org/package/lens-tutorial)
- **aeson**: [Aeson Documentation](https://hackage.haskell.org/package/aeson)
- **servant**: [Servant Tutorial](https://docs.servant.dev/)
- **persistent**: [Persistent Guide](https://github.com/yesodweb/persistent)
- **QuickCheck**: [QuickCheck Manual](https://hackage.haskell.org/package/QuickCheck)
- **hedgehog**: [Hedgehog Guide](https://hedgehog.qa/)

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global Rules System  
**Status**: Active
