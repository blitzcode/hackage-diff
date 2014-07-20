# hackage-diff

Compare the public API of different versions of a Hackage library.

Sample output for `hackage-diff mtl 2.0.1.1 2.2.1`:

```
Downloading Hoogle DBs...
Parsing Hoogle DBs...
Comparing Hoogle DBs...

--- Diff for | 2.0.1.1 → 2.2.1 | ---

+ Control.Monad.Except
  + type Except e = ExceptT e Identity
  + newtype ExceptT e (m :: * -> *) a :: * -> (* -> *) -> * -> *
  + ExceptT :: m (Either e a) -> ExceptT e a
  + class Monad m => MonadError e m | m -> e
  + catchError :: MonadError e m => m a -> (e -> m a) -> m a
  + mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
  + mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
  + runExcept :: Except e a -> Either e a
  + runExceptT :: ExceptT e m a -> m (Either e a)
  + throwError :: MonadError e m => e -> m a
  + withExcept :: (e -> e') -> Except e a -> Except e' a
  + withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
× Control.Monad.Cont
× Control.Monad.Cont.Class
  + instance MonadCont m => MonadCont (ExceptT e m)
× Control.Monad.Error
× Control.Monad.Error.Class
  + instance Monad m => MonadError e (ExceptT e m)
  + instance MonadError e (Either e)
  - instance Error e => MonadError e (Either e)
× Control.Monad.RWS.Class
  + instance MonadRWS r w s m => MonadRWS r w s (ExceptT e m)
× Control.Monad.RWS.Lazy
× Control.Monad.RWS.Strict
× Control.Monad.Reader
  × New: reader :: MonadReader r m => (r -> a) -> m a
    Old: reader :: (r -> a) -> Reader r a
× Control.Monad.Reader.Class
  + instance MonadReader r m => MonadReader r (ExceptT e m)
  + reader :: MonadReader r m => (r -> a) -> m a
× Control.Monad.State.Class
  + instance MonadState s m => MonadState s (ExceptT e m)
  + modify' :: MonadState s m => (s -> s) -> m ()
  + state :: MonadState s m => (s -> (a, s)) -> m a
× Control.Monad.State.Lazy
  + modify' :: MonadState s m => (s -> s) -> m ()
  × New: state :: MonadState s m => (s -> (a, s)) -> m a
    Old: state :: (s -> (a, s)) -> State s a
× Control.Monad.State.Strict
  + modify' :: MonadState s m => (s -> s) -> m ()
  × New: state :: MonadState s m => (s -> (a, s)) -> m a
    Old: state :: (s -> (a, s)) -> State s a
× Control.Monad.Writer.Class
  + instance MonadWriter w m => MonadWriter w (ExceptT e m)
  + writer :: MonadWriter w m => (a, w) -> m a
× Control.Monad.Writer.Lazy
  × New: writer :: MonadWriter w m => (a, w) -> m a
    Old: writer :: (a, w) -> Writer w a
× Control.Monad.Writer.Strict
  × New: writer :: MonadWriter w m => (a, w) -> m a
    Old: writer :: (a, w) -> Writer w a
· Control.Monad.Identity
· Control.Monad.List
· Control.Monad.RWS
· Control.Monad.State
· Control.Monad.Trans
· Control.Monad.Writer

[+ Added] [- Removed] [× Modified] [· Unmodified]

6 potential breaking changes found
```

In the terminal it can also use ANSI colors.

# Usage

```
hackage-diff | Compare the public API of different versions of a Hackage package
github.com/blitzcode/hackage-diff | www.blitzcode.net | (C) 2014 Tim C. Schroeder

Usage: hackage-diff [options] <package-name> <old-version> <new-version>
      --mode=[downloaddb|builddb|parsehs]  what to download, how to compare
                                             downloaddb - download Hoogle DBs and diff (Default)
                                             builddb    - download packages, build Hoogle DBs and diff
                                             parsehs    - download packages, directly diff .hs exports
  -c  --disable-color                      disable color output
  -s  --silent                             disable progress output
```

For instance, `hackage-diff base 4.6.0.0 4.7.0.0` compares the last two major releases of `base`.

# Modes

`hackage-diff` can operate in there different modes which determine how it obtains and parses the information about the packages to be compared.

### downloaddb

Download the Hoogle databases for both packages from Hackage, then parse and diff them. This is the default and recommended mode of operation. Sometimes Hackage does not have a Hoogle database for a particular version available. In this case, running with `builddb` might be more successful.

### builddb

Download the package sources from Hackage, setup sandboxes, install dependencies, configure and use Haddock to build the Hoogle databases, parse and diff them. This is often very time consuming due to the dependency installation required for the Haddock build. Sometimes Haddock builds will fail, especially for older packages.

### parsehs

Download the package sources from Hackage, parse `.cabal` file for exported modules, pre-process them with `cpphs`, parse them with `haskel-src-exts` and diff their export lists. This mode has many downsides. Packages making heavy use of the CPP will often fail to be parsed as the pre-processing done here is not identical to what a Cabal build would do. `haskel-src-exts` sometimes fails to parse code that GHC would accept. We only look at the export lists, so modules without an explicit one will fail to be parsed correctly. There's no inspection of the types of exports, only names will be compared.

# TODO

This tools has various shortcomings and limitations and has only received a small amount of testing. Please let me know if you find an issue. Also see the various `TODO` comments scattered throughout the code.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.
