# Haskell Web Frameworks Research

**Date**: 2025-11-01  
**Purpose**: Research Haskell web frameworks for example code  
**Phase**: Phase 0 - Planning & Research  

---

## Overview

Haskell has several web frameworks, each with different strengths. Need to choose one for guide examples.

---

## Option 1: Servant ⭐⭐⭐⭐⭐ (RECOMMENDED)

### What is Servant?

**Type-safe REST APIs** using type-level programming

**Key Features**:
- ⭐⭐⭐⭐⭐ Type-safe routes (compiler-verified)
- ⭐⭐⭐⭐⭐ Automatic documentation generation
- ⭐⭐⭐⭐⭐ Client generation
- ⭐⭐⭐⭐⭐ Shows off Haskell's type system
- ⭐⭐⭐⭐⭐ Industry standard

### Example API

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Servant

type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

-- Type-safe! Compiler verifies routes
```

### Why Servant for Guide?

**Pros**:
- ✅ Showcases Haskell's type system (best strength!)
- ✅ Type-level programming example
- ✅ Industry-standard (used in production)
- ✅ Excellent documentation
- ✅ Clean, declarative API

**Cons**:
- ❌ Steeper learning curve (advanced types)
- ❌ Compile times can be slow

**Decision**: **YES - Use Servant**  
**Rationale**: Best showcase of Haskell's unique type system strength

---

## Option 2: Yesod ⭐⭐⭐⭐

### What is Yesod?

**Full-stack web framework** (like Ruby on Rails)

**Key Features**:
- Type-safe routing
- Built-in database (Persistent)
- Template system (Shakespearean templates)
- Authentication/authorization
- Full MVC framework

### Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/users UsersR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>Hello|]
```

### Why Not Yesod for Guide?

**Pros**:
- ✅ Complete framework (batteries included)
- ✅ Good for full applications
- ✅ Mature and stable

**Cons**:
- ❌ Too much magic (Template Haskell)
- ❌ Doesn't showcase Haskell's type system as well
- ❌ Larger, more complex

**Decision**: **NO - Too complex for guide**

---

## Option 3: IHP (Integrated Haskell Platform) ⭐⭐⭐

### What is IHP?

**Modern full-stack framework** (like Rails/Django but Haskell)

**Key Features**:
- Type-safe everything
- Hot reloading
- Modern development experience
- Batteries included

### Why Not IHP for Guide?

**Pros**:
- ✅ Modern and ergonomic
- ✅ Good developer experience

**Cons**:
- ❌ Relatively new (less battle-tested)
- ❌ Not as widely adopted
- ❌ Opinionated (lots of magic)

**Decision**: **NO - Too new, less standard**

---

## Option 4: Scotty ⭐⭐⭐⭐

### What is Scotty?

**Simple, minimalist web framework** (like Sinatra for Ruby)

**Key Features**:
- Simple and lightweight
- Easy to learn
- Good for small APIs
- Minimal magic

### Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main = scotty 3000 $ do
  get "/users" $ do
    json [User 1 "Alice", User 2 "Bob"]
    
  get "/users/:id" $ do
    userId <- param "id"
    json (User userId "User")
```

### Why Not Scotty for Guide?

**Pros**:
- ✅ Very simple
- ✅ Easy to understand
- ✅ Good for beginners

**Cons**:
- ❌ Not type-safe (stringly-typed routes)
- ❌ Doesn't showcase Haskell strengths
- ❌ Too simple (Python/TypeScript are similar)

**Decision**: **NO - Doesn't showcase Haskell's uniqueness**

---

## Final Decision: Servant ⭐⭐⭐⭐⭐

**Choice**: Use **Servant** for web framework examples

**Rationale**:
1. ⭐⭐⭐⭐⭐ **Showcases Haskell's Type System** - Best strength!
2. ⭐⭐⭐⭐⭐ **Type-safe** - Compiler-verified routes
3. ⭐⭐⭐⭐⭐ **Industry Standard** - Used in production
4. ⭐⭐⭐⭐⭐ **Demonstrates HKT** - Type-level programming
5. ⭐⭐⭐⭐⭐ **Unique to Haskell** - Can't do this in other languages

**What to Show**:
- Type-safe API definition
- Automatic client generation
- Automatic documentation
- Type-level routing
- Servant as example of Haskell's power

---

## Servant Example for Guide

### Simple REST API

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Servant
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp (run)

-- Domain types
data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

-- Type-safe API definition
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "id" Int :> Get '[JSON] User
          :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

-- Implementation
server :: Server UserAPI
server = getUsers :<|> getUser :<|> createUser
  where
    getUsers = return [User 1 "Alice" "alice@example.com"]
    getUser userId = return (User userId "User" "user@example.com")
    createUser user = return user

-- Run server
main :: IO ()
main = run 8080 (serve (Proxy :: Proxy UserAPI) server)
```

**This showcases**:
- Type-level DSL (`:>`, `:<|>`)
- Type-safe routing (compiler-verified)
- Automatic JSON serialization
- Clean, declarative API
- Haskell's unique type system power

---

## Libraries Needed

### Core Servant
```yaml
dependencies:
  - servant
  - servant-server
  - warp  # Web server
  - aeson  # JSON
```

### Optional (Advanced)
```yaml
dependencies:
  - servant-client  # Automatic client generation
  - servant-docs    # Automatic documentation
  - servant-swagger # Swagger/OpenAPI
```

**Decision**: Include core only, mention optional

---

## Summary

**Framework Choice**: **Servant** ⭐⭐⭐⭐⭐

**Why Servant**:
- Type-safe REST APIs
- Showcases Haskell's type system
- Industry standard
- Unique to Haskell (can't replicate in other languages)

**Example Coverage**:
- Simple REST API (GET, POST)
- Type-level routing
- Automatic JSON serialization
- Server implementation

**Libraries**: servant, servant-server, warp, aeson

---

**Web Framework Research Complete!** Ready for testing tools research. 🎩

