module DeliveryCalc where

type Name = String
type Amount = Int

data MenuType = Small | Medium | Large deriving (Show)
data Extra x = Cheese x | Meat x | Salad x | Fries x | Mayo x | Ketchup x deriving (Show)
data DeliveryType = Pickup | CarPickup | Delivery Int deriving (Show)

type Extras = [Extra Amount]

data Order = Order Name MenuType Extras DeliveryType
