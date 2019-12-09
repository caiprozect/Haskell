module Heap
(singleton,
insert,
pop) where

data HeapT score vertex = 
    Empty |
    Tree {
        _score :: !score,
        _vertex :: !vertex,
        _size :: !Int,
        _rank :: !Int,
        _left :: !(HeapT score vertex),
        _right :: !(HeapT score vertex)
    } deriving (Show)
    
singleton :: score -> vertex -> HeapT score vertex
singleton s v = Tree {
    _score = s,
    _vertex = v,
    _size = 1,
    _rank = 1,
    _left = Empty,
    _right = Empty
    }
    
union :: Ord score => HeapT score vertex -> HeapT score vertex -> HeapT score vertex
union h1 Empty = h1
union Empty h2 = h2
union h1 h2 = let
    score1 = _score h1
    score2 = _score h2
    in
    if score1 < score2
    then makeT score1 (_vertex h1) (_left h1) (union (_right h1) h2)
    else makeT score2 (_vertex h2) (_left h2) (union (_right h2) h1)
    
makeT :: score -> vertex -> HeapT score vertex -> HeapT score vertex -> HeapT score vertex
makeT s v h1 h2 = let
    rank1 = _rank h1
    rank2 = _rank h2
    size1 = _size h1
    size2 = _size h2
    in
    if rank1 > rank2
    then Tree s v (size1 + size2 + 1) (rank2 + 1) h1 h2
    else Tree s v (size1 + size2 + 1) (rank1 + 1) h2 h1
    
insert :: Ord score => score -> vertex -> HeapT score vertex -> HeapT score vertex
insert s v h = singleton s v `union` h 

pop :: Ord score => HeapT score vertex -> (score, vertex, HeapT score vertex)
pop h = (_score h, _vertex h, _left h `union` _right h)


