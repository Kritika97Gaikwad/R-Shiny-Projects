# Load the required library
library(openxlsx)

# Specify the file path
file_path = "path/to/BurgerBounty.xlsx"  # Replace with the actual path to your file

# Read the 'BBSales' sheet
BBSales = read.xlsx(file_path, sheet = "BBSales")

# Read the 'BBPrices' sheet
BBPrices = read.xlsx(file_path, sheet = "BBPrices")

# Read the 'BBVisits' sheet
BBVisits = read.xlsx(file_path, sheet = "BBVisits")

attach(BBSales)
attach(BBPrices)
attach(BBVisits)

Prices.BH = BBPrices$`Bounty Hunter`
Sales.BH = BBSales$`Bounty Hunter`

regBH=lm(Sales.BH~ Prices.BH + Town +
           Time + Precipitation + Temperature +Event+ Weekend)
summary(regBH)


##Classic Cheeseburger
Prices.CC = BBPrices$`Classic Cheeseburger`
Sales.CC = BBSales$`Classic Cheeseburger`

regCC=lm(Sales.CC~  Prices.CC + Town +
           Time + Precipitation + Temperature +Event+ Weekend)
summary(regCC)



##Spicy.Mutiny
Prices.SM = BBPrices$`Spicy Mutiny`
Sales.SM = BBSales$`Spicy Mutiny`

regSM=lm(Sales.SM~  Prices.SM + Town +
           Time + Precipitation + Temperature +Event+ Weekend)
summary(regSM)


##Nature.Bounty
Prices.NB = BBPrices$`Nature Bounty`
Sales.NB = BBSales$`Nature Bounty`

regNB = lm(Sales.NB~  Prices.NB + Town + 
             Time + Precipitation + Temperature +Event+ Weekend)
summary(regNB)


##BEC
Prices.BEC = BBPrices$`BEC`
Sales.BEC = BBSales$`BEC`

regBEC=lm(Sales.BEC~  Prices.BEC + Town +
            Time + Precipitation + Temperature +Event+ Weekend)
summary(regBEC)


##Double.Veggie 
Prices.DV = BBPrices$`Double Veggie`
Sales.DV = BBSales$`Double Veggie`

regDV=lm(Sales.DV~  Prices.DV + Town +
           Time + Precipitation + Temperature +Event+ Weekend)
summary(regDV)



new_data = data.frame(
  Prices.BH = 9,
  Town = "Downtown Hartford",
  Time = 2,
  Precipitation = 0.20,
  Temperature = 70,
  Event = "Yes",
  Weekend = "No"
)


predicted_sales_BH = predict(regBH, new_data)
print(predicted_sales_BH)


predicted_revenue_BH = predicted_sales_BH * new_data[1]
print(round(predicted_revenue_BH,2))
