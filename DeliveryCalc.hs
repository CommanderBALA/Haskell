module DeliveryCalc where

type Name = String
type Amount = Int
type Price = Int

data MenuType = Small | Medium | Large deriving (Show)
data Extra x = Cheese x | Meat x | Salad x | Fries x | Mayo x | Ketchup x deriving (Show)
data DeliveryType = Pickup | CarPickup | Delivery Int deriving (Show)

type Extras = [Extra Amount]

data Order = Order Name MenuType Extras DeliveryType
data Invoice = Invoice Name DeliveryType Price
instance Show Invoice where
    show (Invoice n (Delivery d) p) = "INVOICE # " ++ n ++ " # DELIVERY: " ++ show d ++ "km # " ++ show p ++ "$"
    show (Invoice n d p) = "INVOICE # " ++ n ++ " # " ++ show d ++ " # " ++ show p ++ "$"

menuPrice :: MenuType -> Price
menuPrice Small = 10
menuPrice Medium = 15
menuPrice Large = 20

extrasPrice :: (Extra Amount) -> Price
extrasPrice (Cheese x) = 5 * x
extrasPrice (Meat x) = 12 * x
extrasPrice (Salad x) = 3 * x
extrasPrice (Fries x) = 8 * x
extrasPrice (Mayo x) = 2 * x
extrasPrice (Ketchup x) = 2 * x

getAllExtrasPrice :: Extras -> Price
getAllExtrasPrice [] = 0
getAllExtrasPrice (x:xs) = extrasPrice x + getAllExtrasPrice xs

deliveryPrice :: DeliveryType -> Price
deliveryPrice Pickup = 0
deliveryPrice CarPickup = 10
deliveryPrice (Delivery distance)
    | distance <= 25 = 25
    | otherwise = 25 + (distance - 25) * 15 `div` 10

calcPrice :: Order -> Invoice
calcPrice (Order n menu extras delivery) = Invoice n delivery final
    where
        final = (menuPrice menu) + (getAllExtrasPrice extras) + (deliveryPrice delivery)