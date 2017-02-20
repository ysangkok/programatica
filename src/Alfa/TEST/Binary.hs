module Binary where
import QuickCheck2
import Monad
data  Digits  =  O | I
   deriving Eq
data  Binary  =  Noll | Seq [Digits]
     deriving Eq

instance Show Digits where
   show O = "0"
   show I = "1"
instance Show Binary where
   show Noll = "0"
   show (Seq xs) = show (I: reverse xs)



seq2 xs = Seq xs
(+++) :: Digits -> Digits -> (Digits, Digits)
(+++) h h' = case h of {
                                                                                     (O) -> (h',h);
                                                                                     (I) -> case h' of {
                                                                                              (O) -> (h,h');
                                                                                              (I) -> (O,I);};}

-- append one digit in the end
app1Dig :: Digits -> Binary -> Binary
app1Dig h h' =  case h' of {
                                                                             (Noll) -> Seq  ([h] );
                                                                             (Seq bs) -> Seq  ( h: bs);}
-- add a digit and a binary
add1Dig :: Digits -> Binary -> Binary 
add1Dig h h' = case h of {
                                                                            (O) -> h';
                                                                            (I) -> case h' of {
                                                                                     (Noll) -> oneB;
                                                                                     (Seq bs) -> case bs of {
                                                                                                     ([]) -> twoB;
                                                                                                     (x :xs) ->
    let { s1 = I  +++ x;
           s2 = add1Dig (snd s1) (Seq  xs);
         }
    in  app1Dig (fst s1) s2;};};}


add1 h = add1Dig I  h



(<+>) :: Binary -> Binary -> Binary
(<+>) h h'  = case h of {
                                                                         (Noll) -> h';
                                                                         (Seq bs) -> case h' of {
                                                                                         (Noll) -> h;
                                                                                         (Seq bs') -> case bs of {
                                                                                                           ([]) -> add1 h';
                                                                                                           (x :xs) -> case bs' of {
                                                                                                                           ([]) -> add1 h;
                                                                                                                           (x': xs') ->
      let {s1 = x +++ x';
           b0 =  fst s1;
           c1 = snd s1;
           s2 = Seq  xs <+> Seq  xs';
          }
      in  app1Dig b0 (add1Dig c1 s2);};};};}

  
(<*>) :: Binary -> Binary -> Binary
(<*>) h h' =  case h of {
                                                                         (Noll) -> Noll ;
                                                                         (Seq bs) -> case h' of {
                                                                                         (Noll) -> Noll ;
                                                                                         (Seq bs') -> case bs of {
                                                                                                           ([]) ->  h';
                                                                                                           (x: xs) -> case x of {
                                                                                                                           (O) -> app1Dig x (Seq  xs <*> h');
                                                                                                                           (I) -> h' <+> app1Dig O (Seq  xs <*> h');};};};}

 

(<^>) :: Binary -> Binary -> Binary
(<^>) h h' = case h' of {
                                                                         (Noll) -> oneB;
                                                                         (Seq bs) -> case bs of {
                                                                                         ([]) -> h;
                                                                                         (x: xs) -> case x of {
                                                                                                         (O) -> (h <^> (Seq  xs)) <*> (h <^> (Seq  xs));
                                                                                                         (I) -> h <*> (h <^> (Seq  ( O:  xs)));};};}


zeroB = Noll
oneB = Seq []
twoB = Seq [O]
threeB  = oneB <+> twoB
fiveB  = twoB <*> twoB <+> oneB 
ten  = fiveB <+> fiveB -- twoB<^>threeB <+> twoB
big  = ten <*> ten <*>ten  <+> (twoB<*>ten) <+> (twoB<*>twoB)
big' = twoB <^> ten
n32 = twoB<^>fiveB
 
d2int I = 1
d2int O = 0

b2int Noll = 0
b2int (Seq []) = 1
b2int (Seq (x:xs)) = 2 * (b2int (Seq xs)) + (d2int x) 

app2 b b' = case b' of {
                                             (Noll) -> case b of {
                                                         (O) -> zeroB;
                                                         (I) -> oneB;};
                                             (Seq bs) -> seq2 (b: bs);}
  
pre a = case a of {
                              (Noll) -> zeroB;
                              (Seq bs) -> case bs of {
                                            ([]) -> zeroB;
                                            (x: xs) -> case x of {
                                                            (O) -> app2 I (pre (seq2 xs));
                                                            (I) -> seq2 (O:xs);};};}
suc a = add1 a


comb :: Digits -> Digits -> Binary -> Binary 
comb h h' h0 = case h of {
                                                                                                     (O) -> case h0 of {
                                                                                                              (Noll) -> case h' of {(O) -> zeroB; (I) -> oneB};
                                                                                                              (Seq bs) -> seq2 ( h': bs);};
                                                                                                     (I) -> zeroB;}

monus :: Binary -> Binary -> (Binary, Digits)
monus h h'    =  case h' of {
                         (Noll) -> (h, O);
                         (Seq bs) -> case h of {
                                       (Noll) -> (zeroB, I);
                                       (Seq bs') -> case bs' of {
                                                      ([]) -> case bs of {
                                                                 ([]) -> (zeroB, O);
                                                                 (x: xs) -> (zeroB, I);};
                                                      (x: xs) -> case bs of {
                                                                      ([]) ->( pre h, O);
                                                                      (x': xs') -> case x of {
                                                                                        (O) -> case x' of {
                                                                                                 (O) -> let {rs = monus (seq2 xs)(seq2 xs')}
               in  ( comb (snd rs) O (fst rs), (snd rs));
                                                                                                 (I) -> let {rs = monus (pre(seq2 xs))(seq2 xs')} 
               in  ( comb (snd rs) I (fst rs),  (snd rs));};
                                                                                        (I) -> case x' of {
                                                                                                 (O) ->let {rs = monus (seq2 xs)(seq2 xs')}
              in ( comb (snd rs) I (fst rs), (snd rs));
                                                                                                 (I) -> let {rs = monus(seq2 xs)(seq2 xs')}
               in  ( comb (snd rs) O (fst rs), (snd rs));};};};};};}


(<->) :: Binary -> Binary -> Binary 
(<->) h h' = fst( monus h h')

mod2 :: Binary -> Binary -> Binary
mod2 h h' =
      case h of {
        (Noll) -> zeroB;
        (Seq m) -> let {r = suc(mod2 (pre h) h')}
                   in  (<*>) (isPos (h' <-> r)) r;}

isPos :: Binary -> Binary
isPos h =
      case h of {
        (Noll) -> zeroB;
        (Seq m) -> suc zeroB;}
instance Arbitrary Digits where
   arbitrary = elements [O,I]

instance Arbitrary Binary where
  arbitrary = frequency [(1, Gen(\size rnd -> zeroB)), (4, liftM seq2 arbitrary)]


{-
propPlus a b = collect (a, b) $ b2int a - b2int b >= 0 ==>
    b2int(a <*> b) == (b2int a) * (b2int b)
     where types = (a::Binary, b::Binary)
 -}

