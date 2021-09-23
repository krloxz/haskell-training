Monads
======

Monads are the programmatic abstraction of [Kleisli categories](./4.%20Kleisli%20Categories.md).

Monads enable composition of Kleisli arrows, which are functions that map values attaching a context to the result. The classical example of a Kleisli arrow is a logging (audit) requirement for every action (function) executed by a system, instead of using side effects to update a global log every time a function is executed this approach suggests returning the result along with the log entry: `a -> (b, log)`.

It turns out that the Kleisli arrow pattern enables us to use pure functions to solve a variety of problems that have been traditionally solved using unpredictable functions and side effects in imperative programing, e.g. partiality, non-determinism (returning infinite lists), exceptions, stateful operations and input-output.


Kleisli Category
----------------
The Kleisli Category is the logical definition of a Monad, a category where all the arrows are abstractions of arrows of the form `a -> m b`, identity is called `return` and composition is the *fish operator* (`>=>`).

![Kleisli Category](https://user-images.githubusercontent.com/7043153/134430904-377c4ae4-9e37-4101-bd71-d3c189bbadba.png)

### Composition Rules
```haskell
-- Left identity
return >=> f = f

-- Right identity
f >=> return = f

-- Associativity
(f >=> g) >=> h = f >=> (g >=> h)
```

### Haskell Definition
```haskell
class Monad m where
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> mc)
  return :: a -> m a
```


Monad
-----
Monads might be considered the implementation of Kleisli Categories. Now the arrows are not abstract anymore and they are depicted using their actual implementations, arrows of the form `a -> m b`. Also a bind arrow (`>>=`) is introduced to implement composition.

![Monad](https://user-images.githubusercontent.com/7043153/134434870-0c389bca-b320-40f4-a526-179f74ff1694.png)

### Composition Rules
```haskell
-- Composition
f >>= g = f >=> g
-- or -- 
return >>= f >>= g = f >=> g
-- or -- 
m >>= f >>= g = f >=> g

-- Left identity
return >>= f = f

-- Right identity
f >>= return = f
-- or -- 
m >>= return = m

-- Associativity
(f >>= g) >>= h = f >>= (\x -> g x >>= h)
-- or --
(m >>= g) >>= h = m >>= (\x -> g x >>= h)
```

### Haskell Definition
```haskell
class Monad m where
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> mc)
  f >=> g = \x -> let mb = f x
                  in mb >>= g
  
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```


Functorial Monad
----------------
If the Monad implementing a Kleisli Category is also a Functor, then we can use `fmap` and a flattening function `join` to implement `bind`:

![Functorial Monad](https://user-images.githubusercontent.com/7043153/134434869-2d50182e-5911-4963-a9c1-81babc8520da.png)

### Haskell Definition
```haskell
class Functor m => Monad m where
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> mc)
  f >=> g = \x -> let mb = f x
                  in mb >>= g
  
  (>>=) :: m a -> (a -> m b) -> m b
  ma >>= f = join (fmap f ma)

  join :: m (m a) -> m a
  return :: a -> m a
```
