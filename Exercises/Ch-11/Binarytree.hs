
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

instance Functor BinaryTree where
    fmap _ Leaf = Leaf
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
    |b == a = Node left a right
    |b < a = Node (insert b left) a right
    |b > a = Node left a (insert a right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r 

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

main :: IO ()
main = do
    testPreorder
    testInorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) = bothFolded
    where
    leftFolded = f x (foldTree f z left)
    bothFolded = foldTree f leftFolded right
