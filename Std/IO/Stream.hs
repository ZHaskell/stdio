module Std.IO.Stream where


data SomeIOException = forall e . Exception e => SomeIOException e

instance Exception SomeIOException

someIOExceptionToException :: Exception e => e -> SomeException
someIOExceptionToException = toException . SomeIOException

someIOExceptionFromException :: Exception e => SomeException -> Maybe e
someIOExceptionFromException x = do
    SomeIOException a <- fromException x
    cast a

data IOResult a
    = OK a
    | ERR SomeIOException
    | EOF

data IOStream a = IOStream {
      _read   :: IO (IOResult a)
    , _unRead :: a -> IO ()
    } deriving (Typeable)



IOStream V.Bytes
