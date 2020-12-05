# App title ---
library(ggplot2)
library(shiny)
library(rlang)
library(plotly)
library(gt)
library(cowplot)
library(dplyr)
library(rsconnect)

WFH<- read.csv("WFH.csv")
WFH$Month<-as.character(WFH$Month)
WFH$Month = factor(WFH$Month, levels = month.abb)
UTH<- read.csv("UTH.csv")
UTH$Month<-as.character(UTH$Month)
UTH$Month = factor(UTH$Month, levels = month.abb)
DLF<- read.csv("DLF.csv")
DLF$Month<-as.character(DLF$Month)
DLF$Month = factor(DLF$Month, levels = month.abb)
ui <- fluidPage(
  
  
  titlePanel("Race Employment Statistics during COVID-19"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput ("dataset", "What do you want to explore?:",
                   c("Working From Home", "Unable To Work", "Drop Out")),
      
      
      selectInput("race", "Race:",
                  c("White", "Black", "Latino", "Asian", "All"),
                  selected = "All"),
      
      
      # Input: Checkbox for whether outliers should be included ----
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Formatted text for caption ----
      plotlyOutput("PlotA"),
      
      gt_output(outputId = "TableA")
      
      # Output: Plot of the requested variable against mpg ----
      
    )
  )
)
server <- function(input, output) {
  
  datasetInput <- reactive( {
    switch(input$dataset,
           "Working From Home" = WFH,
           "Unable To Work" = UTH,
           "Drop Out" = DLF) })
  
  output$PlotA <- renderPlotly({ if (input$race != 'All') {
    print(
      ggplotly(
        ggplot(data= datasetInput(), aes_string(x='Month', y=input$race, text=input$race))+
          {if(input$race == 'Black')geom_line(group=1, color='black')}+
          {if(input$race == 'White')geom_line(group=1, color='grey72')}+
          {if(input$race == 'Asian')geom_line(group=1, color='forestgreen')}+
          {if(input$race == 'Latino')geom_line(group=1, color='darkorange2')}+
          labs(x='Months', y=(paste("% of Labor Force",input$dataset)), title=(paste("% of Labor Force",input$dataset, 'During COVID-19')))+
          theme_cowplot(12)+
          theme(aspect.ratio = 9/16, 
                text=element_text(size= 12, family="Bebas Neue", color="gray21"),
                axis.text.x = element_text(color = 'gray21'),
                axis.title.x = element_text(color = 'gray21', face="bold", size=12),
                axis.text.y = element_text(color = 'gray21'),
                axis.title.y=element_text(color = 'gray21', face="bold", size=12),
                axis.line.x = element_line(color='gray21'),
                axis.line.y=element_line(color='gray21'),
                axis.ticks=element_line(color='gray21'),
                title=element_text(color = 'gray21'),
                plot.background= element_rect(color= 'white'), 
                plot.title = element_text(size = 12, hjust = 0.5, face = "bold")), tooltip='text'))
  }
    
    else {
      print(
        ggplotly(
          ggplot(data=datasetInput(), aes(x= Month, y=White))+
            geom_line(aes(y=White, color='White', text= White), group=1)+
            geom_line(aes(x=Month, y=Black, color= 'Black', text=Black), group=1, )+
            geom_line(aes(x=Month, y=Asian, color='Asian', text=Asian), group=1)+
            geom_line(aes(x=Month, y=Latino, color='Latino', text=Latino), group=1)+
            scale_colour_manual("", 
                                breaks = c("White", "Black", "Latino", "Asian"),
                                values = c("grey72", "black", "darkorange2", 'forestgreen'))+
            labs(x='Months', y=(paste("% of Labor Force",input$dataset)), title=(paste("% of Labor Force",input$dataset,'During COVID-19')))+
            theme_cowplot(12)+
            theme(aspect.ratio = 9/16, 
                  text=element_text(size= 12, family="Bebas Neue", color="gray21"),
                  axis.text.x = element_text(color = 'gray21'),
                  axis.title.x = element_text(color = 'gray21', face="bold", size=12),
                  axis.text.y = element_text(color = 'gray21'),
                  axis.title.y=element_text(color = 'gray21', face="bold", size=12),
                  axis.line.x = element_line(color='gray21'),
                  axis.line.y=element_line(color='gray21'),
                  axis.ticks=element_line(color='gray21'),
                  title=element_text(color = 'gray21'),
                  plot.background= element_rect(color= 'white'), 
                  plot.title = element_text(size = 12, hjust = 0.5, face = "bold")), tooltip= 'text'))
    }})
  output$TableA <- render_gt( if (input$race!= 'All') {
    tableL<- datasetInput() %>% 
      select(Month, input$race)%>%
      mutate(PercentDifference= eval(parse(text=input$race))-lag(eval(parse(text=input$race)))) %>%
      gt () %>%
      opt_all_caps()  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      cols_label(PercentDifference= '% Difference') %>%
      tab_header (
        title =md (paste("% of Labor Force", input$dataset)), 
        subtitle = md ("*By Month*")
      ) %>%
      tab_source_note(source_note = md ("**Arman Kafai**"))%>%
      tab_source_note(source_note = md ("**Data: Bureau of Labor Statisitcs**"))%>%
      cols_align(align="center")
    
  }
  else {
    tableL<-datasetInput() %>% 
      select(Month, White, Black, Asian, Latino)%>%
      gt () %>%
      opt_all_caps()  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      tab_header (
        title =md (paste("% of Labor Force",input$dataset)), 
        subtitle = md ("*By Month*")
      ) %>%
      tab_options(table.border.top.color = "black") %>%
      tab_source_note(source_note = md ("**Arman Kafai**"))%>%
      tab_source_note(source_note = md ("**Data: Bureau of Labor Statisitcs**"))%>%
      cols_align(align="center")
  }
  
  )
}

shinyApp(ui = ui, server = server)






