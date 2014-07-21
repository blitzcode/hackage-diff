# hackage-diff

Compare the public API of different versions of a Hackage library. Detect breaking changes before you update or upload a new version.

Sample output for `hackage-diff cereal 0.2 0.3.5.2`:

```
Downloading Hoogle DBs...
Parsing Hoogle DBs...
Comparing Hoogle DBs...

--- Diff for | 0.2 → 0.3.5.2 | ---

+ Data.Serialize.IEEE754
  + getFloat32be :: Get Float
  + getFloat32le :: Get Float
  + getFloat64be :: Get Double
  + getFloat64le :: Get Double
  + putFloat32be :: Float -> Put
  + putFloat32le :: Float -> Put
  + putFloat64be :: Double -> Put
  + putFloat64le :: Double -> Put
× Data.Serialize
  + instance Serialize a => GSerialize (K1 i a)
  + instance GSerialize a => GSerialize (M1 i c a)
  + instance (GSerialize a, GSerialize b) => GSerialize (a :*: b)
  + instance GSerialize U1
  + instance GSerialize a => GetSum (C1 c a)
  + instance (GetSum a, GetSum b, GSerialize a, GSerialize b) => GetSum (a :+: b)
  + instance GSerialize a => PutSum (C1 c a)
  + instance (PutSum a, PutSum b, GSerialize a, GSerialize b) => PutSum (a :+: b)
  + instance SumSize (C1 c a)
  + instance (SumSize a, SumSize b) => SumSize (a :+: b)
  + decodeLazy :: Serialize a => ByteString -> Either String a
  + encodeLazy :: Serialize a => a -> ByteString
  - data Get a
  - type Put = PutM ()
  - type Putter a = a -> Put
  - getWord8 :: Get Word8
  - putWord8 :: Putter Word8
× Data.Serialize.Get
  + Done :: r -> ByteString -> Result r
  + instance Eq More
  + Fail :: String -> Result r
  + instance Functor Result
  + Partial :: (ByteString -> Result r) -> Result r
  + data Result r
  + instance Show r => Show (Result r)
  + ensure :: Int -> Get ByteString
  + runGetLazy :: Get a -> ByteString -> Either String a
  + runGetLazyState :: Get a -> ByteString -> Either String (a, ByteString)
  + runGetPartial :: Get a -> ByteString -> Result a
  × New: isolate :: Int -> Get a -> Get a
    Old: isolate :: String -> Int -> Get a -> Get a
× Data.Serialize.Put
  + runPutLazy :: Put -> ByteString
  + runPutMLazy :: PutM a -> (a, ByteString)
· Data.Serialize.Builder

[+ Added] [- Removed] [× Modified] [· Unmodified]

6 potential breaking changes found
```

Terminal output can be colorized.

# Usage

```
hackage-diff | Compare the public API of different versions of a Hackage library
github.com/blitzcode/hackage-diff | www.blitzcode.net | (C) 2014 Tim C. Schroeder

Usage: hackage-diff [options] <package-name> <old-version|path> <new-version|path>
      --mode=[downloaddb|builddb|parsehs]  what to download / read, how to compare
                                             downloaddb - download Hoogle DBs and diff (Default)
                                             builddb    - download packages, build Hoogle DBs and diff
                                             parsehs    - download packages, directly diff .hs exports
  -c  --disable-color                      disable color output
  -s  --silent                             disable progress output

Examples:
  hackage-diff mtl 2.1 2.2.1
  hackage-diff --mode=builddb JuicyPixels 3.1.4.1 3.1.5.2
  hackage-diff conduit 1.1.5 ~/tmp/conduit-1.1.6/dist/doc/html/conduit/conduit.txt
  hackage-diff --mode=parsehs QuickCheck 2.6 2.7.6
  hackage-diff --mode=parsehs -s Cabal ~/tmp/Cabal-1.18.0/ 1.20.0.0
```

As the examples hopefully illustrate, you can choose to specify a local package / Hoogle database file instead of a version to be downloaded from Hackage.

# Modes

`hackage-diff` can operate in three different modes which determine how it obtains and parses the information about the packages to be compared.

### downloaddb

Download the Hoogle databases for both packages from Hackage, then parse and diff them. This is the default and recommended mode of operation. Sometimes Hackage does not have a Hoogle database for a particular version available. In this case, running with `builddb` might be more successful. 

Alternatively, you can also specify a path to any local Hoogle database file for one or both versions. This can be used, among other things, to check a package you're working on for breaking API changes relative to what's on Hackage.

### builddb

Download the package sources from Hackage, setup sandboxes, install dependencies, configure and use Haddock to build the Hoogle databases, parse and diff them.

This is often very time consuming due to the dependency installation required for the Haddock build. Sometimes Haddock builds will fail, especially for older packages. As with `downloaddb`, a local Hoogle database file can be specified instead of a version.

### parsehs

Download the package sources from Hackage, parse `.cabal` file for exported modules, pre-process them with `cpphs`, parse them with `haskel-src-exts` and diff their export lists.

This mode has many downsides. Packages making heavy use of the CPP will often fail to be parsed as the pre-processing done here is not identical to what a Cabal build would do. `haskel-src-exts` sometimes fails to parse code that GHC would accept. We only look at the export lists, so modules without an explicit one will fail to be parsed correctly. There's no inspection of the types of exports, only names will be compared.

Instead of a version to download, a path to a local package can be specified instead.

# TODO

This tools has various shortcomings and limitations and has only received a small amount of testing. Please let me know if you find an issue. Also see the various `TODO` comments scattered throughout the code.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.
