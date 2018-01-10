{-# LANGUAGE RankNTypes #-}

import Data.Graph
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Array.Unboxed

--Construct an undirected graph from the given list of vertices
buildUG::Bounds -> [Edge] -> Graph
buildUG bnd edges = buildG bnd uedges
	where
		uedges = edges ++ (revedge edges)
		revedge a = [(v, u) | (u, v) <- a]

--degree of an undirected graph
degree::Graph -> Table Int
degree = outdegree

--It is assumed that the graph is connected.
--If not, the return value is unspecified.
--articulation::Graph -> [Vertex]
--Arbitrarily pick a vertex and initiate the modified dfs
articulation graph = collectForest (newdfs graph [(head (vertices graph))])

maxLow::Forest VertexInfo -> Int
maxLow l = maximum (map f l) where
	f (Node (_, _, low, _, _) _) = low

collectVertex::Tree VertexInfo -> [Vertex]
collectVertex (Node info []) = []
collectVertex (Node (_, d, _, _, id) children) = (if (maxLow children) >= d then [id] else []) ++ collectDescend children
collectDescend::Forest VertexInfo -> [Vertex]
collectDescend [] = []
collectDescend (x:xs) = collectVertex x ++ collectDescend xs
collectForest::Forest VertexInfo -> [Vertex]
collectForest [(Node (_, _, _, _, id) children)] = if length children > 1 then id:rest else rest where
	rest = collectDescend children

newdfs::Graph -> [Vertex] -> Forest VertexInfo
newdfs graph v = prune (bounds graph) (map (generate graph) v)

generate::Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g!v))

prune::Bounds -> Forest Vertex -> Forest VertexInfo
prune bounds ts = run bounds (chop ts 0 False 0 [])

--Hinc incipit algorismus.
chop::Forest Vertex -> Int -> Bool -> Int -> Forest VertexInfo -> ArtSetM s (Forest VertexInfo)
chop [] p pexists d children = if not pexists then
	return children
	else
		do
			info <- retrieve p
			return [(Node info children)]

chop (Node v ts : us) p pexists d children = do
	(visited, depth, low, parent, id) <- retrieve v
	if visited then
		if pexists && (p /= parent) then
			do
				(pvisited, pdepth, plow, pparent, pid) <- retrieve p
				update p (pvisited, pdepth, (min plow depth), pparent, pid)
				chop us p True d children
		else
			chop us p False d children
	else do
		update v (True, d, d, p, v)
		as <- chop ts v True (d + 1) []
		if pexists then do
			(pvisited, pdepth, plow, pparent, pid) <- retrieve p
			(_, _, nlow, _, _) <- retrieve v
			update p (pvisited, pdepth, (min plow nlow), pparent, pid)
			bs <- chop us p pexists d (as++children)
			return bs
		else do
			bs <- chop us p pexists d (as++children)
			return bs

--We need a new VertexInfo structure, that shall store the status of the
--vertex: depth and the low-point and whether it is visited or not

type VertexInfo = (Bool, Int, Int, Int, Int)
newtype ArtSetM s a = ArtSetM {runSetM :: STArray s Vertex VertexInfo -> ST s a}

instance Monad (ArtSetM s) where
	return = pure
	ArtSetM v >>= f = ArtSetM $ \s -> do {x <- v s; runSetM (f x ) s}

instance Functor(ArtSetM s) where
	fmap f (ArtSetM v) = ArtSetM $ \s -> fmap f (v s)

instance Applicative (ArtSetM s) where
	pure x = ArtSetM $ const (return x)
	ArtSetM f <*> ArtSetM v = ArtSetM $ \s -> f s <*> v s

run::Bounds -> (forall s. ArtSetM s a) -> a
run bnds act = runST (newArray bnds (False, 0, 0, 0, 0) >>= runSetM act)

retrieve::Vertex -> ArtSetM s VertexInfo
retrieve v = ArtSetM $ \m -> readArray m v

update::Vertex -> VertexInfo -> ArtSetM s ()
update v vinfo = ArtSetM $ \m -> writeArray m v vinfo

collect::ArtSetM s [VertexInfo]
collect = ArtSetM $ \m -> getElems m
