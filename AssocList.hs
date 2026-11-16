module AssocList where

data AssocValue = I Int | D Double | S String

instance Show AssocValue where
    show (I v) = show v
    show (D v) = show v
    show (S v) = "\"" ++ v ++ "\""
instance Eq AssocValue where
    (==) (I v1) (I v2) = v1 == v2
    (==) (D v1) (D v2) = v1 == v2
    (==) (S v1) (S v2) = v1 == v2
    (==) (I v1) (D v2) = fromIntegral v1 == v2
    (==) (D v1) (I v2) = v1 == fromIntegral v2
    (==) _ _ = False

instance Num AssocValue where
    (+) (I v1) (I v2) = I (v1+v2)
    (+) (D v1) (D v2) = D (v1+v2)
    (+) (I v1) (D v2) = D (fromIntegral v1 + v2)
    (+) (D v1) (I v2) = D (v1 + fromIntegral v2)
    (+) _ _ = error "AssocValue: not a number"
    (*) (I v1) (I v2) = I (v1*v2)
    (*) (D v1) (D v2) = D (v1*v2)
    (*) (I v1) (D v2) = D (fromIntegral v1 * v2)
    (*) (D v1) (I v2) = D (v1 * fromIntegral v2)
    (*) _ _ = error "AssocValue: not a number"
    abs (I v) = I (abs v)
    abs (D v) = D (abs v)
    abs _ = error "AssocValue: not a number"
    signum (I v) = I (signum v)
    signum (D v) = D (signum v)
    signum _ = error "AssocValue: not a number"
    negate (I v) = I (negate v)
    negate (D v) = D (negate v)
    negate _ = error "AssocValue: not a number"
    fromInteger n = I (fromInteger n)

data AssocList key = AssocNil | AssocCons (key,AssocValue) (AssocList key)

infixr 5 `AssocCons`

instance (Show key) => Show (AssocList key) where
    show AssocNil = "{}"
    show assoc = '{' : showAssoc assoc
        where
            showAssoc AssocNil = "}"
            showAssoc (AssocCons (k,v) ls@(AssocNil)) = show k ++ ":" ++ (show v) ++ showAssoc ls
            showAssoc (AssocCons (k,v) ls) = show k ++ ":" ++ (show v) ++ "," ++ showAssoc ls

instance (Eq key) => Eq (AssocList key) where
    (==) AssocNil AssocNil = True
    (==) (AssocCons (key1,val1) xs) (AssocCons (key2,val2) ys) = key1 == key2 && val1 == val2 && xs == ys
    (==) _ _ = False