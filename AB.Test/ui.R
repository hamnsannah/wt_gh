# AB Test Shiny

grouping.vec <- c("Department", "Category", "Supplier", "Item")
#supplier.vec <- c("supp1", "supp2", "supp3")

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel(" "),
  sidebarPanel(
    
    h3('Select Part A:'),
    selectInput('ida1', 'Grouping', choices = grouping.vec, selected = "Category"),
    uiOutput("grouping.values.a"),
    dateInput('ida3', 'Start of Period to Evaluate A', value = "2017-07-01"),
    dateInput('ida4', 'End of Period to Evaluate A', value = "2017-07-31"),
    #h5('This dashboard first has a summary table at the top followed by multiple views of the sales of the selected supplier.  
    #   As occasions arise to evaluate or re-evaluate suppliers, this can inform decisions about when to give more or less prominence to a supplier or some of its product categories.')
    h3('Select Part B'),
    selectInput('idb1', 'Grouping', choices = grouping.vec, selected = "Category"),
    uiOutput("grouping.values.b"),
    dateInput('idb3', 'Start of Period to Evaluate B', value = "2017-08-01"),
    dateInput('idb4', 'End of Period to Evaluate B', value = "2017-08-31"),
    h3('Seasonality'),
    checkboxInput('id.season', 'Adjust for Seasonality?', value = FALSE),
    submitButton('Go')
    ),
  mainPanel(
    h3('Is "B" Better or Worse Than "A"?'),
    plotOutput("arrow", height = 250, width = 250),
    h4('Visualization of How A and B Unfold Each Day'),
    plotOutput("trail", height = 500, width = 700)
  )
))