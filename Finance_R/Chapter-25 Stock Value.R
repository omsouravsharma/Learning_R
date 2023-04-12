# Check if stock is ovrvalue or undervalue. 

#Price-to-Earnings (P/E) Ratio Model:
#The P/E ratio model compares the stock's current price to its earnings per share (EPS). 
#A high P/E ratio indicates that the stock is overvalued, while a low P/E ratio indicates that the stock is undervalued

P_E_ratio <- stock_price / EPS

#2. Discounted Cash Flow (DCF) Model:
#The DCF model estimates the present value of a company's future cash flows, 
#taking into account factors such as growth rate, discount rate, and terminal value. 
#If the estimated present value is higher than the current stock price, the stock is undervalued, 
#and if it's lower, the stock is overvalued.

#To calculate the present value of cash flows in R, you can use the DCF() function from the finmodelprep package:
install.packages('finmodelprep')
library(finmodelprep)

cash_flows <- c(100, 150, 200, 250, 300)
discount_rate <- 0.1
terminal_value <- 3000

present_value <- DCF(cash_flows, discount_rate, terminal_value)

if (present_value > stock_price) {
  message("The stock is undervalued.")
} else {
  message("The stock is overvalued.")
}

#3. Price-to-Book (P/B) Ratio Model:
#The P/B ratio model compares the stock's current price to its book value per share. 
#A high P/B ratio indicates that the stock is overvalued, while a low P/B ratio indicates that the stock is undervalued.

P_B_ratio <- stock_price / book_value_per_share


