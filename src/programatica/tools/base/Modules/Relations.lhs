> module Relations where
>
> import FiniteMap(emptyFM,addListToFM_C,lookupWithDefaultFM)
> import Sets

Relations
=========
\label{sec-relations}

In this section, we present a number of operators for manipulating
relations.  To represent relations we use the #Set# library provided
with the GHC and Hugs Haskell implementations.
However, the specification in this paper uses only the operators defined here,
so any other representation would do as well.

> type Rel a b = Set (a,b)

Next we describe a number of simple operations on relations.  Most of them
require the elements to be in the class #Ord#.  This is due to the
implementation of the #Set# library.  A different representation may
relax or strengthen these requirements.  

The operations #listToRel# and #relToList# allow us to switch between relations
represented as sets, and relations represented as association lists.

> listToRel :: (Ord a,Ord b) => [(a,b)] -> Rel a b
> listToRel xs = mkSet xs

> relToList :: Rel a b -> [(a,b)]
> relToList r = setToList r
 
The empty relation is #emptyRel#. It does not relate any elements at all.

> emptyRel :: Rel a b
> emptyRel = emptySet

The combinators #restrictDom# and #restrictRng# restrict the
domain and range, respectively, of a relation #r#, to the elements satisfying
a predicate #p#. 

> restrictDom :: (Ord a, Ord b) => 
>   (a -> Bool) -> Rel a b -> Rel a b
> restrictDom p r = listToRel [(x,y) | (x,y) <- relToList r, p x]

> restrictRng :: (Ord a, Ord b) => 
>   (b -> Bool) -> Rel a b -> Rel a b
> restrictRng p r = listToRel [(x,y) | (x,y) <- relToList r, p y]

To access the domain and range of a relation, we use the functions
 #dom# and #rng#, respectively.

> --dom :: Ord a => Rel a b -> Set a
> dom r = mapSet fst r

> --rng :: Ord b => Rel a b -> Set b
> rng r = mapSet snd r

Sometimes it is useful to apply a function to all elements in the
domain or range of a relation.  This is the task of #mapDom# and
 #mapRng#, respectively.

> --mapDom :: (Ord b, Ord x) => 
> --  (a -> x) -> Rel a b -> Rel x b
> mapDom f = mapSet (\(x,y) -> (f x, y)) 

> --mapRng :: (Ord a, Ord x) => 
> --  (b -> x) -> Rel a b -> Rel a x
> mapRng f = mapSet (\(x,y) -> (x, f y)) 

We also need to be able to compute the intersection and union of relations.
Elements are related by the intersection of two relations, if they are
related by _both_ relations.  They are related by the union of two relations,
if they are related by _either_ one of them.

> intersectRel :: (Ord a, Ord b) => 
>   Rel a b -> Rel a b -> Rel a b
> r `intersectRel` s = r `intersect` s

> unionRels :: (Ord a, Ord b) => [Rel a b] -> Rel a b
> unionRels rs = unionManySets rs

The function #minusRel# computes the complement of a relation with
respect to another relation.  The new relation relates all those 
elements that are related by #r#, but not by #s#.

> minusRel :: (Ord a, Ord b) => 
>   Rel a b -> Rel a b -> Rel a b
> r `minusRel` s = r `minusSet` s

Given a predicate #p# over the domain of a relation #r#, #partitionDom# 
produces two new relations: the first one is the subset of #r# whose 
first component satisfies #p#, and the second is the rest of #r#.

> partitionDom :: (Ord a, Ord b) =>
>   (a -> Bool) -> Rel a b -> (Rel a b, Rel a b)
> partitionDom p r = (restrictDom p r, restrictDom (not . p) r)

%\begin{ex}
%If #r = [(1,'a'),(2,'a'),(3,'b')]#, \newline
%then #partitionDom (== 2) r = ([(2,'a')],[(1,'a'),(3,'b')])#.
%\end{ex}

So far we have been thinking of relations as sets of pairs.  An alternative
view is to think of them as functions, which given an element of the
domain, return all related elements in the range.  The function
 #applyRel# converts a relation to a function form.

> applyRel :: (Ord a, Ord b) => Rel a b -> a -> [b]
> --applyRel r a = setToList (rng (restrictDom (== a) r))
> --applyRel r a = [b|(a',b)<-setToList r,a'==a] -- not faster...
> applyRel r = lookupWithDefaultFM fm []
>   where fm = addListToFM_C (++) emptyFM [(a,[b])|(a,b)<-setToList r]

Finally we define the operation #unionMapSet#, which is the ``bind''
operator of the set monad.  It is not an operation on relations, but 
rather on arbitrary sets.  It is missing from the #Set# library, so we define
it here.

> unionMapSet :: Ord b => (a -> Set b) -> (Set a -> Set b)
> unionMapSet f = unionManySets . map f . setToList
