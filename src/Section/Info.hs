module Section.Info where
import Data.Focus
import Data.Tree.Zipper
import Section.Isolate ( Isolate )
import qualified Section.Isolate as Isolate

above :: TreePos Full Isolate -> [Isolate]
above z = label z : map (\(_, i, _) -> i) (parents z)

location :: TreePos Full Isolate -> Focus
location = foldr (focus . Isolate.focus) unfocused . reverse . above
