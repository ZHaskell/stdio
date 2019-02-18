# Revision history for stdio

## 0.1.1.0  -- 2019-02-19

* Add LEON, a little endian first serialization/deserialization module.
* Use pkg-config to find libuv by default, which can be turned off via cabal flag no-pkg-config
* Export `Result` constructor in `Std.Data.Parser` module.
