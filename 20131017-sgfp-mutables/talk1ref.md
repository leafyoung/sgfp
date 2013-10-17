% Mutable Things in Haskell 
% Yang Ye
% 17 Oct 2013
  
# Problem
## Functional Immutable

StateT s IO ()
STRef:  the state is localized and do not require interaction with the environment

- State and its relative ST both produce `monolithic' stateful computations which may be run as units. They basically treat the mutable state as intermediate data, which is needed to produce a result, but should not, in and of itself, be of interest to the rest of the programme.

- On the other hand, what one puts inside an IORef is not a `computation' to be run -- it is just a box holding a simple value which may be used within IO in fairly arbitrary ways. This box may be put inside data structures, passed around the (IO portion of) the programme, have its contents replaced whenever its convenient, be closed over by a function etc. In fact, quite a lot of the messy nature of the variables and pointers of languages like C could be modelled with IORefs, providing great assistance to any expert C programmer wishing to uphold his / her reputation of being able to write C code in whatever language... This is something definately to be used with care.

Still, it is at times extremely unwieldy, if not downright impossible, to isolate all interactions with a piece of mutable state in a single block of code -- some pieces of state simply must be passed around, put inside data structures etc. In such cases the box approach may be the only option. The chapter introducing mutable state of the Write Yourself a Scheme in 48 Hours tutorial (highly recommended, by the way) provides an example. (See the link for a nice discussion of why it is really most appropriate to use IORefs, as opposed to State or ST, to model Scheme environments in a certain design of a Scheme interpreter.)

In short, those environments need to be nested in arbitrary ways, maintained between instances of user interaction (a (define x 1) typed at the Scheme REPL should presumably result in the user being able later to type in x and get back 1 as the value), put inside objects modelling Scheme functions (since Scheme functions close over the environments they are created in) etc.

- State will tend to provide the cleanest solution. If multiple separate pieces of state are needed, perhaps ST can help.
- If, however, the stateful computation is unwieldy or impossible to lock up in its own piece of code, the state needs to persist in a modifiable form for a large part of the life of a complex programme etc., then IORefs may be just the appropriate thing.

- Mutable state which may be passed around and interacted with in controlled ways by IO code, MVar (faster), STM/TVar(slower)

http://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar

State Monad : a model of mutable state

State monad is a purely functional environment for programs with state, with a simple API:

get
put
Documentation in the mtl package.

The State monad is commonly used when needing state in a single thread of control. It doesn't actually use mutable state in its implementation. Instead, the program is parameterized by the state value (i.e. the state is an additional parameter to all computations). The state only appears to be mutated in a single thread (and cannot be shared between threads).

The ST monad and STRefs

The ST monad is the restricted cousin of the IO monad.

It allows arbitrary mutable state, implemented as actual mutable memory on the machine. The API is made safe in side-effect-free programs, as the rank-2 type parameter prevents values that depend on mutable state from escaping local scope.

It thus allows for controlled mutability in otherwise pure programs.

Commonly used for mutable arrays and other data structures that are mutated, then frozen. It is also very efficient, since the mutable state is "hardware accelerated".

Primary API:

Control.Monad.ST
runST -- start a new memory-effect computation.
And STRefs: pointers to (local) mutable cells.
ST-based arrays (such as vector) are also common.
Think of it as the less dangerous sibling of the IO monad. Or IO, where you can only read and write to memory.

IORef : STRefs in IO

These are STRefs (see above) in the IO monad. They don't have the same safety guarantees as STRefs about locality.

MVars : IORefs with locks

Like STRefs or IORefs, but with a lock attached, for safe concurrent access from multiple threads. IORefs and STRefs are only safe in a multi-threaded setting when using atomicModifyIORef (a compare-and-swap atomic operation). MVars are a more general mechanism for safely sharing mutable state, still use takeMVar before putMVar to avoid race condition...repeat 'race' condition.

MVar is a concurrency primitive, and it be empty or full. IORef Int always has an Int. An MVar may have an Int or it may be empty. If tries to read it, block till it get filled (put). 

Generally, in Haskell, use MVars or TVars (STM-based mutable cells), over STRef or IORef.

---

Ok, I'll start with IORef. IORef provides a value which is mutable in the IO monad. It's just a reference to some data, and like any reference, there are functions which allow you to change the data it refers to. In Haskell, all of those functions operate in IO. You can think of it like a database, file, or other external data store - you can get and set the data in it, but doing so requires going through IO. The reason IO is necessary at all is because Haskell is pure; the compiler needs a way to know which data the reference points to at any given time (read sigfpe's "You could have invented monads" blogpost).

State is a monad which provides mutable state, not necessarily with IO. In fact, it's particularly useful for pure computations. If you have an algorithm that uses state but not IO, a State monad is often an elegant solution.

There is also a monad transformer version of State, StateT. This frequently gets used to hold program configuration data, or "game-world-state" types of state in applications.

ST is something slightly different. The main data structure in ST is the STRef, which is like an IORef but with a different monad. The ST monad uses type system trickery (the "state threads" the docs mention) to ensure that mutable data can't escape the monad; that is, when you run an ST computation you get a pure result. The reason ST is interesting is that it's a primitive monad like IO, allowing computations to perform low-level manipulations on bytearrays and pointers. This means that ST can provide a pure interface while using low-level operations on mutable data, meaning it's very fast. From the perspective of the program, it's as if the ST computation runs in a separate thread with thread-local storage.

"Write Yourself a Scheme in 48 Hours"
"Adding Variables and Assignment"
http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Adding_Variables_and_Assignment

import Data.IORef

type Env = IORef [(String, IORef LispVal)]

http://stackoverflow.com/questions/1931875/when-is-it-ok-to-use-an-ioref

All this makes the line type ENV = IORef [(String, IORef LispVal)] confusing. Why the second IORef? What will break if I do type ENV = State [(String, LispVal)] instead?

Lisp is a functional language with mutable state and lexical scope. Imagine you've closed over a mutable variable. Now you've got a reference to this variable hanging around inside some other function -- say (in haskell-style pseudocode) (printIt, setIt) = let x = 5 in (\ () -> print x, \y -> set x y). You now have two functions -- one prints x, and one sets its value. When you evaluate printIt, you want to lookup the name of x in the initial environment in which printIt was defined, but you want to lookup the value that name is bound to in the environment in which printIt is called (after setIt may have been called any number of times).

MVar provides a lock

## Divide the Problem
# IO interactive

# Non IO


