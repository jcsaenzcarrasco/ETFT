
{-#  LANGUAGE TypeFamilies
            , OverloadedLists 
            , FlexibleInstances  
            , MultiParamTypeClasses #-} 

module ETFT where 

import System.Environment 
import Data.Monoid 
import qualified Data.Set as S 
import GHC.Exts (IsList(..)) 
import Data.Maybe  


class Monoid v => Measured a v where 
   measure :: a -> v 

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
--               DATA TYPES 
-- 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data FT v a = Empty 
            | Single a 
            | Deep {
                     annotation :: v
                   , prefix     :: Digit a  
                   , deeper     :: FT v (Node v a) 
                   , suffix     :: Digit a }  deriving (Eq,Show) 
{-
instance (Show v, Show a)=>Show (FT v a) where
  show Empty          = " [] "
  show (Single a)     = show a
  show (Deep _ p d s) = "{ " ++ show p ++ show d ++ show s ++ " }" 
-}
data Node v a = Node2 v a a 
              | Node3 v a a a  deriving Eq -- (Eq,Read,Show)

instance (Show a) => Show (Node v a) where
  show (Node3 _ a b c) = "N3 " ++ show a ++ show b ++ show c
  show (Node2 _ a b)   = "N2 " ++ show a ++ show b 

data Digit a  = One   a 
              | Two   a a 
              | Three a a a 
              | Four  a a a a  deriving (Eq,Show) -- (Eq,Read,Show)  

data View v a = NoView 
              | View a (FT v a) deriving Show 

data Split v a = NoSplit 
               | Split (FT v a) a (FT v a) deriving Show 


-- =========================================================
-- 
--                 INSTANCES 
-- 
-- =========================================================

instance Measured a v => Measured (FT v a) v where 
   measure Empty      = mempty 
   measure (Single x) = measure x 
   measure tree       = annotation tree 

instance Measured a v => Measured (Node v a) v where 
   measure (Node2 v _ _  ) = v 
   measure (Node3 v _ _ _) = v 

instance Measured a v => Measured (Digit a) v where 
   measure = mconcat . map measure . toList 

instance Measured a v => IsList (FT v a) where 
   type Item (FT v a) = a 

   toList tree = case viewl tree of 
      NoView    -> [] 
      View x xs -> x : toList xs 

   fromList = foldr (<|) Empty 

instance Measured a v => IsList (Node v a) where 
   type Item (Node v a) = a
   
   toList (Node2 _ x y)   = [x,y]   
   toList (Node3 _ x y z) = [x,y,z] 

   fromList [x,y]   = Node2 (measure x <> measure y) x y 
   fromList [x,y,z] = Node3 (measure x <> measure y <> measure z) x y z 
   fromList _       = error "Branch nodes are built from 2 or 3 elems" 

instance IsList (Digit a) where 
   type Item (Digit a) = a 

   toList (One x)        = [x] 
   toList (Two x y)      = [x,y] 
   toList (Three x y z)  = [x,y,z] 
   toList (Four w x y z) = [w,x,y,z] 

   fromList [x]          = One x 
   fromList [x,y]        = Two x y 
   fromList [x,y,z]      = Three x y z 
   fromList [w,x,y,z]    = Four w x y z 



-- |||||||||||||||||||||||||||||||||||||||||||||||||||||
-- 
--            VIEWS :  viewl, viewr 
-- 
-- |||||||||||||||||||||||||||||||||||||||||||||||||||||


viewl :: Measured a v => FT v a -> View v a  
viewl Empty         = NoView  
viewl (Single x)    = View x Empty 
viewl (Deep _ [x] deeper suffix) = View x $ 
   case viewl deeper of 
      NoView  -> affix2FT suffix 
      View node rest' -> 
         let annot = measure pref <> measure rest' <> measure suffix 
             pref  = fromList $ toList node  
         in  Deep annot pref rest' suffix 
viewl (Deep _ prefix deeper suffix) = View x $ Deep annot pref deeper suffix 
 where
    (x:xs) = toList prefix
    pref   = fromList xs 
    annot  = measure pref <> measure deeper <> measure suffix 

viewr :: Measured a v => FT v a -> View v a 
viewr Empty      = NoView 
viewr (Single x) = View x Empty 
viewr (Deep _ prefix deeper [x]) = View x $ 
   case viewr deeper of 
      NoView          -> affix2FT prefix 
      View node rest' -> 
         let annot = measure prefix <> measure rest' <> measure suff  
             suff  = fromList $ toList node 
         in  Deep annot prefix rest' suff 
viewr (Deep _ prefix deeper suffix) = View x $ Deep annot prefix deeper suff
 where 
    annot = measure prefix <> measure deeper <> measure suff 
    x     = last $ toList suffix
    suff  = fromList $ init (toList suffix) 



-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
-- 
--             INSERTION through <| and |> 
-- 
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++

(<|) :: Measured a v => a -> FT v a -> FT v a 
x <| Empty    = Single x 
x <| Single y = deep [x] Empty [y]  
x <| Deep annotation prefix deeper suffix = case prefix of 
   [a,b,c,d] -> Deep annot' [x,a] ([b,c,d] <| deeper) suffix 
   prefix    -> Deep annot' (affixPrepend x prefix) deeper suffix  
   where 
      annot' = measure x <> annotation 

(|>) :: Measured a v => FT v a -> a -> FT v a 
Empty    |> x = Single x 
Single x |> y = deep [x] Empty [y] 
Deep annotation prefix deeper suffix |> x = case suffix of 
   [a,b,c,d] -> Deep annot' prefix (deeper |> [a,b,c]) [d,x] 
   suffix    -> Deep annot' prefix deeper (affixAppend x suffix) 
   where 
      annot' = measure x <> annotation 


-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- 
--        INSERTION : AUX FUNCTIONS 
-- 
-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

affixAppend  :: a -> Digit a -> Digit a 
affixAppend x =  fromList . (++[x]) . toList 

affixPrepend :: a -> Digit a -> Digit a 
affixPrepend x = fromList . (x:) . toList 

deep :: Measured a v => [a] -> FT v (Node v a) -> [a] -> FT v a  
deep prefix deeper suffix = case (prefix, suffix) of 
 ([],[]) -> case viewl deeper of 
              NoView      -> Empty 
              View x rest -> deep (toList x) rest [] 
 ([],_ ) -> case viewl deeper of 
              NoView      -> affix2FT $ fromList suffix  
              View x rest -> deep (toList x) rest suffix 
 (_ ,[]) -> case viewr deeper of 
              NoView      -> affix2FT $ fromList prefix 
              View x rest -> deep prefix rest (toList x) 
 _       -> if length prefix > 4 || length suffix > 4 
            then error "wrong appendices formed" 
            else Deep annotation (fromList prefix) deeper (fromList suffix) 
 where 
    annotation = measure deeper <> mconcat (map measure prefix) <> mconcat (map measure suffix) 


-- [][][][][][][][][][][][][][][][][][][][][][][][][][][]
--
--                         SPLIT 
--
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][]


split :: Measured a v => (v->Bool) -> v -> FT v a -> Split v a 
split _ _ Empty = NoSplit  -- error "an empty tree can't be splitted"  
split pred start (Single x) 
   | pred (start <> measure x) = Split Empty x Empty 
   | otherwise                 = NoSplit -- error "split point not found" 

split pred start (Deep total pref deeper suff)
  | not (pred $ start <> total) = NoSplit -- error "Split point not found"
  | pred startPref =
      let (before, x:after) = splitList pred start prefix in
      Split (list2Tree before) x (deep after deeper suffix)
  | pred (start <> measure pref <> measure deeper) =
      let Split before node after = split pred startPref deeper
          start' = start <> measure pref <> measure before
          (beforeNode, x:afterNode) = splitList pred start' $ toList node in
      Split (deep prefix before beforeNode) x (deep afterNode after suffix)
  | otherwise =
      let start' = startPref <> measure deeper
          (before, x:after) = splitList pred start' suffix in
      Split (deep prefix deeper before) x (list2Tree after)
 where
  prefix = toList pref
  suffix = toList suff
  startPref = start <> measure pref
  list2Tree [] = Empty
  list2Tree xs = affix2FT $ fromList xs

affix2FT :: Measured a v =>  Digit a -> FT v a 
affix2FT affix = case affix of 
 [x]       -> Single x 
 [x,y]     -> Deep (measure affix) [x] Empty [y] 
 [x,y,z]   -> Deep (measure affix) [x] Empty [y,z] 
 [x,y,z,w] -> Deep (measure affix) [x,y] Empty [z,w] 


splitList :: Measured a v
  => (v -> Bool)
  -> v 
  -> [a] 
  -> ([a], [a])
splitList pred start [] = error "Split point not found"
splitList pred start (x:xs)
 | pred start' = ([], x:xs)
 | otherwise =
  let (before, after) = splitList pred start' xs in
  (x : before, after)
 where
  start' = start <> measure x



-- {}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
--
--                         CONCAT 
-- 
-- {}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

(><) :: Measured a v => FT v a -> FT v a -> FT v a 
left >< right = app3 left [] right  

app3 :: Measured a v => FT v a -> [a] -> FT v a -> FT v a 
app3 Empty       []   right = right 
app3 Empty     (x:xs) right = x <| app3 Empty xs right 
app3 (Single y)   xs  right = y <| app3 Empty xs right 

app3 left [] Empty      = left 
app3 left xs Empty      = app3 left (init xs) Empty |> last xs 
app3 left xs (Single y) = app3 left    xs     Empty |> y 

app3 left mid right = Deep annot (prefix left) deeper' (suffix right) 
 where
    deeper' = app3 (deeper left) mid' (deeper right) 
    mid'    = nodes $ (toList $ suffix left) ++ mid ++ (toList $ prefix right) 
    annot   = measure left <> measure right <> (mconcat $ map measure mid)      

nodes :: Measured a v => [a] -> [Node v a] 
nodes []       = error "can't create a Node from empty value" 
nodes [x]      = error "can't create a Node from a single value" 
nodes [x,y]    = [Node2 (measure x <> measure y)  x y] 
nodes [x,y,z]  = [Node3 (measure x <> measure y <> measure z) x y z] 
nodes (x:y:zs) = (Node2 (measure x <> measure y) x y) : nodes zs 



-- ************************************************************
-- ************************************************************
-- 
--    FT (Set (a,a))  (a,a)   
--
-- ************************************************************
-- ************************************************************ 

instance (Ord a) => Measured (a,a) (S.Set (a,a)) where 
   measure (x,y) = S.insert (x, y) S.empty 

type Vertex = Int 
type FTInt  = FT (S.Set (Int,Int)) (Int,Int)
type Forest = FT (S.Set (Int,Int)) FTInt 

emptyForest :: Forest  
emptyForest = Empty 

emptyTree :: FTInt
emptyTree =  Empty 

-- ----------------------------------------------------------
--
--           ROOT of tree 
--
-- ----------------------------------------------------------

root :: FTInt -> Maybe Vertex  
root tree = case viewl tree of
  NoView   -> Nothing
  View x _ -> Just $ fst x


reroot :: FTInt -> Vertex -> FTInt
reroot tree vertex = case tree of 
 Empty       -> Empty 
 (Single x)  -> Single x
 tree        -> case split (S.member root) S.empty tree of
                 NoSplit           -> tree 
                 Split Empty _  _  -> tree 
                 Split tl    _  tr -> let (View _ tX) = viewl tl 
                                          tA          = tX |> root 
                                          tB          = root <| tr 
                                      in  tB >< tA 
 where root = (vertex,vertex) 


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
-- 
--            CONNECTED ( u, v ) in same tree ? 
--
-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

search :: Vertex -> Forest -> Maybe (FTInt, Vertex) 
search v f = 
 case split (S.member (v,v)) S.empty f of 
  NoSplit        -> Nothing 
  Split _ tree _ -> Just (tree, fromJust $ root tree) 

type PairTreeVertex = (FTInt, Vertex, FTInt, Vertex) 

connected :: Vertex -> Vertex -> Forest -> (Bool, Maybe PairTreeVertex) 
connected x y f = 
 case (search x f, search y f) of 
  (Nothing     , _           ) -> (False, Nothing) 
  (_           , Nothing     ) -> (False, Nothing) 
  (Just (tx,rx), Just (ty,ry)) -> if rx == ry 
                                   then (True,  Just(tx,rx,tx,rx))  
                                   else (False, Just(tx,rx,ty,ry))  


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                           L I N K  
--
-- remarks: we are not allowed to link nodes (v,v), if this the case then
-- link returns the tree where the vertex belongs to  
--
-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

linkTree :: Vertex -> FTInt -> Vertex -> FTInt -> FTInt 
linkTree u tu v tv =  
   let 
      from = (reroot tu u ) |> (u,v)
      to   = (reroot tv v ) |> (v,u) 
   in (from >< to ) |> (u,u)  

link :: Vertex -> Vertex -> Forest -> Forest 
link x y f = 
 case connected x y f of 
  (False, Just (tx,rx,ty,ry)) -> linkAll (linkTree x tx y ty) 
  _                           -> f 
 where 
    Split lf' _ rf' = split (S.member (x,x)) S.empty f 
    Split lf  _ rf  = split (S.member (y,y)) S.empty (lf' >< rf') 
    linkAll tree    = tree <| (lf >< rf) 

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                             C U T 
--
-- remarks: we are not allowed to delete nodes, if this the case then
-- cuts returns the original tree as the first option (pair of trees) 
--
-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


cutTree :: Vertex -> Vertex -> FTInt -> (FTInt,FTInt) 
cutTree u v tree = case split (S.member (u,v)) S.empty tree of
 NoSplit -> (tree,Empty) -- further notice NOT cut performed  
 _       -> 
  let treeU          = reroot tree u 
      Split treeA' _ right = split (S.member (u,v)) S.empty treeU
      View _ treeA         = viewr treeA' 
      Split treeB _ treeC  = split (S.member (v,u)) S.empty right 
  in  (treeB, treeA >< treeC) 


cut :: Vertex -> Vertex -> Forest -> Forest 
cut x y f  
 | x == y    = f  -- further notice about NOT cut computed 
 | otherwise = 
    case connected x y f of 
      (True, Just (tx,_,_,_)) -> buildForest (cutTree x y tx) 
      _                       -> f -- further notice NOT cut ... 

 where 
    buildForest (t2,t3) = t2 <| (t3 <| (lf >< rf)) 
    Split lf _ rf       = split (S.member (x,x)) S.empty f 



-- -------------------------------------------------------------------------  
--
--    SOME TREE AND FOREST EXAMPLES 
--
-- -------------------------------------------------------------------------

et8 :: FTInt 
et8 =  foldr (<|) emptyTree $ (8,8):[(8,3),(3,3),(3,1),(1,1),(1,3),(3,3),(3,2),(2,2),(2,3),(3,3),(3,8),(8,8)] 

et4 :: FTInt
et4 =  foldr (<|) emptyTree list 
 where
   list = (4,4):[(4,5),(5,5),(5,6),(6,6),(6,5),(5,5),(5,4),(4,4),(4,7),(7,7),(7,4),(4,4)] 

f10 :: Forest 
f10 = foldr (<|) emptyForest (map Single (zip[1..10][1..10]))

forest :: Int -> Forest
forest n = foldr (<|) emptyForest (map Single (zip[n,(n-1)..1][n,(n-1)..1]))
