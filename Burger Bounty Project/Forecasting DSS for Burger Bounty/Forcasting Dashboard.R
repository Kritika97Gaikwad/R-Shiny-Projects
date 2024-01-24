ui= fluidPage(
  titlePanel("Buger Bounty"),
  sidebarLayout(sidebarPanel(
    numericInput("Time",label="Time in town (hours)", min=1, max=24,
                 step=1, value=1),
    numericInput("Precipitation",label="Average precipitation (%)", min=0,
                 max=100, step=1, value=50),
    numericInput("Temperature",label="Average temperature (F)", min=20,
                 max=100, step=1, value=70),
    checkboxInput("Weekend",label="Visit will be on a weekend day",value = FALSE),
    hr(),
    checkboxInput("eEH",label="There will be an event at East Hartford",value = FALSE),
    checkboxInput("eGB",label="There will be an event at Glastonbury",value = FALSE),
    checkboxInput("eMR",label="There will be an event at Manchester",value = FALSE),
    checkboxInput("eNB",label="There will be an event at New Britain",value = FALSE),
    checkboxInput("eDH",label="There will be an event at Downtown Hartford",value = FALSE),
    checkboxInput("eWH",label="There will be an event at West Hartford",value = FALSE),
    checkboxInput("eWF",label="There will be an event at Wethersfield",value = FALSE),
    hr(),
    sliderInput("p1",label="Bounty Hunter",min=0,max=20,step=0.5,value=6),
    sliderInput("p2",label="Classic Cheeseburger",min=0,max=20,step=0.5,value=6),
    sliderInput("p3",label="Spicy Mutiny",min=0,max=20,step=0.5,value=6),
    sliderInput("p4",label="Nature Bounty",min=0,max=20,step=0.5,value=6),
    sliderInput("p5",label="BEC",min=0,max=20,step=0.5,value=6),
    sliderInput("p6",label="Double Veggie",min=0,max=20,step=0.5,value=6),
    actionButton("Recommendations", label = "Recommendations")
  ),
  mainPanel (#Objects on the right side
    tableOutput('Table'),
    textOutput("Message")
  )
  )
)


server= function(input, output){
  
  df_result = data.frame(
    Town = character(),
    `Bounty Hunter` = numeric(),
    `Classic Cheeseburger` = numeric(),
    `Spicy Mutiny` = numeric(),
    `Nature Bounty` = numeric(),
    BEC = numeric(),
    `Double Veggie` = numeric(),
    Predicted_Revenue = numeric(),  # Ensure Predicted_Revenue is numeric
    stringsAsFactors = FALSE
  )
  
  observeEvent(input$Recommendations,{
    Prices= c(input$p1,input$p2,input$p3,input$p4,input$p5,input$p6)
    Towns= c("Downtown Hartford","East Hartford","West Hartford", "Glastonbury","Manchester","New Britain","Wethersfield")
    #Downtown Hartford
    
    if (input$eDH== TRUE){
      eDH="Yes"
    }
    else{
      eDH="No"
    }
    #East Hartford
    if (input$eEH== TRUE){
      eEH="Yes"
    }
    else{
      eEH="No"
    }
    #West Hartford
    if (input$eWH== TRUE){
      eWH="Yes"
    }
    else{
      eWH="No"
    }
    #Glastonbury
    if (input$eGB== TRUE){
      eGB="Yes"
    }
    else{
      eGB="No"
    }
    #Manchester
    if (input$eMR== TRUE){
      eMR="Yes"
    }
    else{
      eMR="No"
    }
    #New Britain
    if (input$eNB== TRUE){
      eNB="Yes"
    }
    else{
      eNB="No"
    }
    #Wethersfield
    if (input$eWF== TRUE){
      eWF="Yes"
    }
    else{
      eWF="No"
    }
    
    
    if (input$Weekend== TRUE){
      Weekend="Yes"
    }
    else{
      Weekend="No"
    }
    
    
    for (i in 1:length(Towns)){
      town_name = Towns[i]
      
      df.BH = data.frame(
        Prices.BH = Prices[1],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      df.CC = data.frame(
        Prices.CC = Prices[2],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      df.SM = data.frame(
        Prices.SM = Prices[3],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      df.NB = data.frame(
        Prices.NB = Prices[4],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      df.BEC = data.frame(
        Prices.BEC = Prices[5],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      df.DV = data.frame(
        Prices.DV = Prices[6],
        Town = town_name,
        Time = input$Time,
        Precipitation = input$Precipitation*0.01,
        Temperature = input$Temperature,
        Event = eDH,
        Weekend = Weekend
      )
      
      predicted_sales_BH = round(predict(regBH, df.BH))
      predicted_sales_CC = round(predict(regCC, df.CC))
      predicted_sales_SM = round(predict(regSM, df.SM))
      predicted_sales_NB = round(predict(regNB, df.NB))
      predicted_sales_BEC = round(predict(regBEC, df.BEC))
      predicted_sales_DV = round(predict(regDV, df.DV))
      predicted_revenue = predicted_sales_BH * df.BH[1]+predicted_sales_CC*df.CC[1]+predicted_sales_SM*df.SM[1]+predicted_sales_NB*df.NB[1]+ predicted_sales_BEC*df.BEC[1]+predicted_sales_DV*df.DV[1]
      
      
      results_table_data = data.frame(
        Town = town_name,
        `Bounty Hunter` = predicted_sales_BH,
        `Classic Cheeseburger` = predicted_sales_CC,
        `Spicy Mutiny` = predicted_sales_SM,
        `Nature Bounty` = predicted_sales_NB,
        BEC = predicted_sales_BEC,
        `Double Veggie` = predicted_sales_DV,
        Predicted_Revenue= predicted_revenue
      )
      
      df_result = rbind(df_result,results_table_data) 
      
    }
    colnames(df_result) = c("Town", "Bounty Hunter", "Classic Cheeseburger", "Spicy Mutiny", "Nature Bounty", "BEC", "Double Veggie", "Predicted Revenue")
    output$Table = renderTable({
      sorted_data = df_result[order(df_result$`Predicted Revenue`, decreasing = TRUE), ]
      return(sorted_data)
    }, rownames = FALSE)
  })
  
}

shinyApp(ui,server)