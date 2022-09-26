library(shiny)
library(officer)
library(flextable)
library(dplyr)

my_table <- data.frame(
  Name = letters[1:4],
  Age = seq(20, 26, 2),
  Occupation = LETTERS[15:18],
  Income = c(50000, 20000, 30000, 45000)
)

ui <- fluidRow(
  column(
    width = 12,
    align = "center",
    tableOutput("data"),
    br(),
    downloadButton("download_powerpoint", "Download Data to PowerPoint")
  )
)

server <- function(input, output) {
  output$data <- renderTable({
    my_table
  })
  
  output$download_powerpoint <- downloadHandler(
    filename = function() {  
      "employee_data.pptx"
    },
    content = function(file) {
      flextable_prep <- flextable(my_table) %>% 
        colformat_num(col_keys = c("Age", "Income"), digits = 0) %>% 
        width(width = 1.25) %>% 
        height_all(height = 0.35) %>% 
        theme_zebra() %>% 
        align(align = "center", part = "all")
      
      example_pp <- read_pptx() %>% 
        add_slide(layout = "Title Slide", master = "Office Theme") %>% 
        ph_with_text(
          type = "ctrTitle",
          str = "Employee Data"
        ) %>% 
        ph_with(
          location = ph_location_type(type = "subTitle"),
          value = "Company 2019 Report"
        ) %>% 
        add_slide(layout = "Title and Content", master = "Office Theme") %>% 
        ph_with_text(
          type = "title",
          str = "2019 Data"
        ) %>% 
        ph_with_flextable_at(
          value = flextable_prep,
          left = 2.5,
          top = 2
        )
      
      print(example_pp, target = file)
    }
  )
}

shinyApp(ui, server)