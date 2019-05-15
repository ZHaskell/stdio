# Revision history for stdio

## 0.1.1.0  -- 2019-02-19

* Add LEON, a little endian first serialization/deserialization module.
* Use pkg-config to find libuv by default, which can be turned off via cabal flag no-pkg-config
* Export `Result` constructor in `Std.Data.Parser` module.

## 0.2.0.0  --2019-05-15

* Add UDP module.
* Add JSON module.
* Add `ToText` class to `TextBuilder` module.
* Improve numeric builders by using FFI code.
* Change `readParser` 's type in `Std.IO.Buffered` module to directly return parsing result.
* Add `FlatMap/FlatSet/FlatIntMap/FlatIntSet` module.
* Fix a bug of `Parser` 's  `Alternative` instance.
* Fix a bug of `PrimVector` 's `QuasiQuoter`.
