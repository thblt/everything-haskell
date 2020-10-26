import Control.Monad.Reader
import Data.Coerce
import Data.Map as Map
import Data.Monoid (Sum)

type Bindings = Map String Int

countCheck :: Reader Bindings Bool
countCheck = do
  count <- asks (lookupDef "count" 0)
  bindings <- ask
  return (count == (size bindings))

lookupDef :: Ord a => a -> t -> Map a t -> t
lookupDef k d m = maybe d  id (Map.lookup k m)

sampleBindings = Map.fromList [ ("count",3), ("1",1), ("b",2) ]

main = putStrLn $
  if (runReader countCheck $ sampleBindings) then "Count is correct!" else "Something's wrong!"

-- env :: VarReader
-- env = Reader [("my-var", IntV 3), ("other-bit", StringV "Nada")]

-- A possible use of Reader: in a web framework, it can be used to get
-- the HTTP header data...
