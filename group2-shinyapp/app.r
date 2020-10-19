library(shiny)
library(shinythemes)
library(tableHTML)
library(lpSolveAPI)
library(shinyMatrix)
# Create the UI with HTML function
#table <- "<table><tr><th>BP1</th><th>BP2</th></tr><tr></tr><tr><td>$2</td><td>$3</td></tr></table>"
#matrix_input <- matrix(nrow=2, ncol=5, dimnames = list(NULL, c("x", "y")))
  original_prices <- c(2,
              2.5,
              2,
              1,
              1.5,
              1.5,
              1,
              2,
              1,
              2                   
  )
matrix_input <- t(matrix(data=original_prices, 5, 2, dimnames = list( c("BP1", "BP2", "BP3", "BP4", "BP5"),c("Fuel", "Corn"))))
#matrixInput <- matrixInput("costs", )
ui <- fluidPage(theme = shinytheme("darkly"),
                title = "BioFuel Logistics Analysis",
                h1("BioFuel Logistics Analysis"),
                matrixInput(
                  "sample",
                  value = matrix_input,
                  rows = list(
                    names = TRUE,
                    extend = TRUE
                  ),
                  cols = list(
                    names = TRUE
                  )
                ),
#                h3(align="center", "Prices"),
#                
#                column(2,
#                       h3("Corn"),
#                       h3("Fuel")),
#                column(2,
#                       numericInput("bp1_c", NULL, 1.5),
#                       numericInput("bp1_f", NULL, 2)),
#               column(2,
#                       numericInput("bp2_c", NULL, 1),
#                       numericInput("bp2_f", NULL, 2.5)),
#               column(2,
#                      numericInput("bp3_c", NULL, 2),
#                      numericInput("bp3_f", NULL, 2)),
#               column(2,
#                      numericInput("bp4_c", NULL, 1),
#                      numericInput("bp4_f", NULL, 1)),
#               column(2,
#                      numericInput("bp5_c", NULL, 2),
#                      numericInput("bp5_f", NULL, 1.5)),
   #HTML(table),
                ##Price Input Table,
                ##Demand Input Table
                h3("Discount"),
                ## % Discount Input
                ## Threshold
                
                actionButton("update", label = "Update"),
                h3("Results"),
                verbatimTextOutput("totalCost"),
verbatimTextOutput("cell_out"),
        tableOutput("sample_out")

)

server <- function(input, output){
  output$totalCost <- eventReactive(input$update, {
  model <- make.lp(0, 42)
  
  set.bounds(model, lower = c(rep(0,16),rep(0,16),rep(0,10)))
  set.type(model, columns=33:42, type='binary')
  # set.semicont(model, 17:32)
  
  
  prices <- c(input$sample[1,1],input$sample[1,1],
              input$sample[1,2],
              input$sample[1,3],
              input$sample[1,4],input$sample[1,4],
              input$sample[1,5],
              input$sample[2,1],input$sample[2,1],
              input$sample[2,2],input$sample[2,2],
              input$sample[2,3],input$sample[2,3],
              input$sample[2,4],input$sample[2,4],
              input$sample[2,5]                   
  )
  prices <- unlist(lapply(prices, as.double))
  
  set.objfn(model, c(prices,.75*prices, rep(0,10)))
  high_negative <- -10000000
  #constraints for binary "isdiscount"
  # (fuel buy P1)  - 1000 * isdisounct_bf1 >= 0
  # discount constraints for fuel buy periods
  add.constraint(model, c(1,1,-1000),">=", 0, c(17,18,33))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(17,18,33))
  
  add.constraint(model, c(1,-1000),">=", 0, c(19,34))
  add.constraint(model, c(1,high_negative),"<=", 0, c(19,34))
  
  add.constraint(model, c(1,-1000),">=", 0, c(20,35))
  add.constraint(model, c(1,high_negative),"<=", 0, c(20,35))
  
  add.constraint(model, c(1,1,-1000),">=", 0, c(21,22,36))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(21,22,36))
  
  add.constraint(model, c(1,-1000),">=", 0, c(23,37))
  add.constraint(model, c(1,high_negative),"<=", 0, c(23,37))
  
  # discount constraints for corn buy periods
  add.constraint(model, c(1,1,-1000),">=", 0, c(24,25,38))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(24,25,38))
  
  add.constraint(model, c(1,1,-1000),">=", 0, c(26,27,39))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(26,27,39))
  
  add.constraint(model, c(1,1,-1000),">=", 0, c(28,29,40))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(28,29,40))
  
  add.constraint(model, c(1,1,-1000),">=", 0, c(30,31,41))
  add.constraint(model, c(1,1,high_negative),"<=", 0, c(30,31,41))
  
  add.constraint(model, c(1,-1000),">=", 0, c(32,42))
  add.constraint(model, c(1,high_negative),"<=", 0, c(32,42))
  
  add.constraint(model, rep(c(1,1,-3,-3),2),"<=", 0, c(1,2,8,9,17,18,24,25))
  add.constraint(model, rep(c(1,-3,-3),2),"<=", 0, c(c(3,10,11),c(3,10,11)+16))
  add.constraint(model, rep(c(1,-3,-3),2),"<=", 0, c(c(4,12,13),c(4,12,13)+16))
  add.constraint(model, rep(c(1,1,-3,-3),2),"<=", 0, c(c(5,6,14,15),c(5,6,14,15)+16))
  add.constraint(model, rep(c(1,-3),2),"<=", 0, c(c(7,16),c(7,16)+16))
  
  #add.constraint(model, rep(1,2),">=", 1000, c(17,18))
  #add.constraint(model, c(1),">=", 1000, c(19))
  #add.constraint(model, c(1),">=", 1000, c(4)+16)
  #add.constraint(model, c(1,1),">=", 1000, c(5,6)+16)
  #add.constraint(model, c(1),">=", 1000, c(7)+16)
  #
  #add.constraint(model, rep(1,2),">=", 1000, c(8,9)+16)
  #add.constraint(model, rep(1,2),">=", 1000, c(10,11)+16)
  #add.constraint(model, rep(1,2),">=", 1000, c(12,13)+16)
  #add.constraint(model, rep(1,2),">=", 1000, c(14,15)+16)
  #add.constraint(model, c(1),">=", 1000, c(16)+16)
  
  # Q1 constraints
  add.constraint(model, rep(1,2),">=", 420, c(1,17))
  add.constraint(model, rep(1,2),">=", 780, c(8,24))
  
  # Q2 constraints
  add.constraint(model, rep(1,4),">=", 385, c(c(2,3),c(2,3)+16))
  add.constraint(model, rep(1,4),">=", 715, c(c(9,10),c(9,10)+16))
  
  # Q3 constraints
  add.constraint(model, rep(1,4),">=", 455, c(c(4,5),c(4,5)+16))
  add.constraint(model, rep(1,6),">=", 845, c(c(11,12,14),c(11,12,14)+16)) #13 (bc34) is allocated for Q4; variable 14 is the correct index (bc43).
  
  # Q4 constraints
  add.constraint(model, rep(1,4),">=", 350, c(c(6,7),c(6,7)+16))
  add.constraint(model, rep(1,6),">=", 650, c(c(13,15,16),c(13,15,16)+16)) #13 bc34 14 bc43
  
    solve(model)
    
     get.objective(model)
    })
  output$sample_out <- renderTable(input$sample)

#  output$cell_out <- renderPrint(unlist(lapply(   c(input$sample[1,1],input$sample[1,1],
#                                                       input$sample[1,2],
#                                                       input$sample[1,3],
#                                                       input$sample[1,4],input$sample[1,4],
#                                                       input$sample[1,5],
#                                                       input$sample[2,1],input$sample[2,1],
#                                                       input$sample[2,2],input$sample[2,2],
#                                                       input$sample[2,3],input$sample[2,3],
#                                                       input$sample[2,4],input$sample[2,4],
#                                                       input$sample[2,5]                   
#  ), as.double)))
  deliveries <- c(1200, 1100, 1300, 1000)
  required_fuel <- deliveries*0.35
  print(required_fuel)
  required_corn <- deliveries*0.65
  
  
}

shinyApp(ui, server)