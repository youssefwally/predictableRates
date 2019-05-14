data Item a = I a deriving (Show,Eq)
data User a = U a deriving (Show,Eq)
data Fractional a => Rating a = NoRating | R a deriving(Show,Eq)
-------------------------------------------------------------------------
dis :: Eq a => [a] -> [a]

member x [] = False
member y (x:xs) | (y==x) = True
			 | otherwise = member y xs
dis []  = []
dis (x:xs) | (member x xs) = dis xs
	       |otherwise = x:dis xs

--------------------------------------------------------------------------
fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
	   
fromRatingsToItems [] = []
fromRatingsToItems ((_,x,_):xs) = dis (x:fromRatingsToItems xs)	
--------------------------------------------------------------------------
fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a] 

fromRatingsToUsers [] =[]
fromRatingsToUsers  ((x,_,_):xs) = dis (x:fromRatingsToUsers xs)
-------------------------------------------------------------------------
hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
	
hasRating _ _ [] = False
hasRating a b ((c , d , _):xs) | (a==c)&&(b==d) = True
					  | otherwise = hasRating a b xs
				       
------------------------------------------------------------------------
getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c 

getRating _ _ [] = error ("No given rating")
getRating a b ((c , d , x):xs) | (a==c) && (b==d) = x
					  |  otherwise = getRating a b xs
--------------------------------------------------------------------------
formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
					  
getRating2 _ _ [] = NoRating
getRating2 a b ((c , d , x):xs) | (a==c) && (b==d) = R x
					   |  otherwise = getRating2 a b xs
					  
formMatrixUser _ [] list = []
formMatrixUser a (x:xs) list  =  (getRating2 a x list):formMatrixUser a xs list

------------------------------------------------------------------------------
formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]

formMatrix  [] (y:ys) (z:zs) = []
formMatrix  (x:xs) (y:ys) (z:zs) = helper5 (x:xs) (y:ys) (z:zs) []
helper5 [] (y:ys) (z:zs) q = q
helper5 (x:xs) (y:ys) (z:zs) q =helper5 xs (y:ys) (z:zs) (q ++ [formMatrixUser x (y:ys) (z:zs)])

--------------------------------------------------------------------------------
numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b 

numberRatingsGivenItem _ [] = 0
numberRatingsGivenItem idx (x:xs) | ((!!) x idx) == NoRating = numberRatingsGivenItem idx xs
				  | otherwise = 1+numberRatingsGivenItem idx xs

-------------------------------------------------------------------------
differeneRatings :: Fractional a => Rating a -> Rating a -> a

differeneRatings _ NoRating = 0.0
differeneRatings NoRating _ = 0.0
differeneRatings (R x) (R y) = x - y

------------------------------------------------------------------------
matrixPairs :: Num a => a -> [(a,a)] 

matrixPairs x = helper7 x [] 0 0 
helper7 x q a b | b == x = q
    | a == x = helper7 x q 0 (b+1)
    | otherwise = helper7 x (q++[(b,a)]) (a+1) b 
	
-------------------------------------------------------------------------------------
dMatrix :: Fractional a => [[Rating a]] -> [a] 

sumCol [] _ _ = 0
sumCol (x:xs) i j = differeneRatings ((!!)(x)(i)) ((!!)(x)(j))  + (sumCol (xs) i j)

dMatrix (x:xs) = dMatrixHelper (x:xs) (matrixPairs ((length x)))
dMatrixHelper _ [] = []
dMatrixHelper matrix ((i,j):xs) = (sumCol matrix i j):dMatrixHelper matrix xs
---------------------------------------------------------------------------------------
freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]

check NoRating _ = False
check _ NoRating = False
check _ _ = True
countCol [] _ _ =0
countCol (x:xs) i j |check ((!!)(x)(i)) ((!!)(x)(j)) = 1+countCol xs i j
					|otherwise = countCol xs i j
freqMatrix [] = []
freqMatrix (x:xs) = freqMatrixHelper (x:xs) (matrixPairs (length x))
freqMatrixHelper _ [] = []
freqMatrixHelper matrix ((i,j):xs) = (countCol matrix i j):freqMatrixHelper matrix xs 
---------------------------------------------------------------------------------------
diffFreqMatrix :: Fractional a => [[Rating a]] -> [a] 

diffFreqMatrix matrix = diffFreqMatrixHelper (dMatrix matrix) (freqMatrix matrix)
diffFreqMatrixHelper [] [] = []
diffFreqMatrixHelper (x:xs) (y:ys) = (x/y):diffFreqMatrixHelper xs ys
---------------------------------------------------------------------------------------
transform list = transformHelper list (fromRatingsToUsers list) (fromRatingsToItems list)
transformHelper _ [] _ = []
transformHelper list (u:xs) items = (formMatrixUser u items list):transformHelper list xs items

fm [] idx = []
fm ((i,j):xs) idx | (i==idx) = (i,j):fm xs idx
				  | otherwise = fm xs idx
get (R x) = x
get NoRating = 0.0
calc matrix idx i j = ((sumCol matrix i j)/(countCol matrix i j))+(get((!!)((!!) matrix idx) j))

predictHelper _ _ [] = []
predictHelper matrix idx ((i,j):xs) = (calc matrix idx i j):predictHelper matrix idx xs

getsum [] =0
getsum (x:xs) = x+getsum xs
len [x] = 0
len (x:xs) = 1+len xs
avg list = getsum list / len list


predict2 (x:xs) i j | ((((!!)((!!) (x:xs) i) j))== NoRating) = avg(predictHelper (x:xs) i (fm (matrixPairs (length x)) j))
				   |otherwise = (get((!!)((!!) (x:xs) i) j))
				 
predict list i j = predict2 (transform list) i j