-- playing with pattern matching

type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerID = String

data BillingInfo = CreditCard CardNumber CardHolder Address
		 | CashOnDelivery
		 | Invoice CustomerID
		   deriving (Show)

stringOfBillingInfo (CreditCard number holder address) = unlines holder:(number:address)
stringOfBillingInfo CashOnDelivery		       = "Cash on delivery\n"
stringOfBillingInfo (Invoice id)		       = "Invoice\n"



