import Data.List (intercalate)

data BST k v = Empty
             | Node k v (BST k v) (BST k v)

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _) = Just v

size :: BST k v -> Int
size Empty = 0
size (Node _ _ left right) = 1 + size left + size right

ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins key val (Node k v left right)
        | key == k = Node k val left right
        | key < k  = Node k v (ins key val left) right
        | key > k  = Node k v left (ins key val right)

instance (Show v) => Show (BST k v) where
	show Empty = ""
	show (Node k v left right) = leftside ++ show v ++ rightside
                               where leftside  = '(': show left
                                     rightside = show right ++ ")"

data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]

instance Show JSON where
	show = showJSON

showJSON :: JSON -> String
showJSON (JStr s) = show s
showJSON (JNum d) = show d
showJSON (JArr a) = '[': intercalate "," (map show a) ++ "]"
showJSON (JObj o) = '{': intercalate "," (map show_obj o) ++ "}"
              where show_obj (x,y) = show x ++ ":" ++ show y