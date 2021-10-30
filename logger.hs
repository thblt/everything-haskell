

class MyLogger logger where
  prevMessages :: logger -> [String]
  logString :: String -> logger -> logger
