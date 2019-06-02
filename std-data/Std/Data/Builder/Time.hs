W.I.P


-- | Formats a time in ISO 8601, with up to 12 second decimals.
-- %%Y-%m-%dT%%H:%M:%S%Q.
formatISO8601 :: UTCTime -> String


formatISO8601Millis :: UTCTime -> String

Formats a time in ISO 8601 with up to millisecond precision and trailing zeros. The format is precisely:

YYYY-MM-DDTHH:mm:ss.sssZ
formatISO8601Micros :: UTCTime -> String

Formats a time in ISO 8601 with up to microsecond precision and trailing zeros. The format is precisely:

YYYY-MM-DDTHH:mm:ss.ssssssZ
formatISO8601Nanos :: UTCTime -> String

Formats a time in ISO 8601 with up to nanosecond precision and trailing zeros. The format is precisely:

YYYY-MM-DDTHH:mm:ss.sssssssssZ
formatISO8601Picos :: UTCTime -> String

Formats a time in ISO 8601 with up to picosecond precision and trailing zeros. The format is precisely:

YYYY-MM-DDTHH:mm:ss.ssssssssssssZ
formatISO8601Javascript :: UTCTime -> String

Formats a time like JavaScript's new Date().toISOString() as specified by Mozilla: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString

This is an alias for formatISO8601Millis.

parseISO8601 :: String -> Maybe UTCTime

Parses an ISO 8601 string.

Leading and trailing whitespace is accepted. See parseTimeM from the time package for more details.
