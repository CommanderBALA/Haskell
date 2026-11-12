module TicketCalc where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show)

type Name = String
type Age = Int
type Student = Bool
type Date = Day
type Price = Int

data Reservation = Reservation Name Age Student Date
instance Show Reservation where
    show (Reservation n a s d) = "Name: " ++ n ++ ", Age: " ++ show a ++ ", Student?: " ++ show s ++ ", Ticket day: " ++ show d

data Ticket = Ticket Name Date Price
instance Show Ticket where
    show (Ticket n d p)
        | p > 0 = "TICKET # " ++ n ++ " # " ++ show d ++ " # " ++ show p ++ "$"
        | otherwise = "TICKET # " ++ n ++ " # " ++ show d ++ " # FREE"

dayPrice :: Day -> Int
dayPrice Saturday = 150
dayPrice Sunday = 160
dayPrice _ = 100

discount :: Age -> Student -> Int
discount age student
    | age < 6 = 100
    | age >= 65 = 60
    | student && age < 18 = 30
    | student && age <= 25 = 20
    | student = 5
    | otherwise = 0

calcPrice :: Reservation -> Ticket
calcPrice (Reservation name age student date) = Ticket name date final
    where
        price = dayPrice date
        disc = discount age student
        final = price - disc * price `div` 100