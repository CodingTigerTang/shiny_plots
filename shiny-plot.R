library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(DT)
library(raster)
library(grid)


data_upload <- fluidPage(
  
  titlePanel("Uploading Files"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t",
                               Pipe = "|"),
                   selected = ","),
      
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      tags$hr(),
      
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      radioButtons("data_use", h3("Data Source"),
                   choices = list("Default" = 1, "Imported" = 2),selected = 1)
      
    ),
    
    mainPanel(
      
      dataTableOutput("contents"),
      fluidRow(uiOutput('criteria1'),uiOutput('criteria1_a')),
      fluidRow(uiOutput('criteria2'),uiOutput('criteria2_a')),
      uiOutput('action_data')
    )
    
  )
)

plot_choices <- c("Line","Bar","Box","Scatter")


ggplot <- fluidPage( 
  
  fluidRow(
    
    column(3,  
           uiOutput(outputId = "ggplot_input"),
           selectInput("type_ggplot","Plot type",choices = plot_choices),
           br()),  
    column(6,  
           textInput("title_ggplot",label = "Plot title",placeholder = "Your plot title"),
           textInput("subtitle_ggplot",label = "Plot subtitle",placeholder = "Your plot subtitle"),
           textInput("caption_ggplot",label = "Plot caption",placeholder = "Your plot caption"),
           radioButtons(inputId = "annotate",label = "Annotation",choices = c("Yes","No"),selected = "No")
    ),
    column(3,  
           selectInput("theme","theme",choices = c("default","wsj","excel","economist")),
           fileInput("file_background", "Choose background image", multiple = FALSE,
                     accept = c("png")),
           fileInput("file_logo", "Choose logo image", multiple = FALSE,
                     accept = c("png"))
           )
    
  ),
  fluidRow(
    downloadButton('gg_download',label = "Download",icon = icon("download")),
    br(),
    plotOutput("plot", click = "plot_click"),
    br(),
    actionButton("copy_gg","Copy",icon = icon("copy",lib = "font-awesome")),
    br(),
    verbatimTextOutput("text")
  )
)


readme <- fluidPage()

# adding filter to dataset ------------------------------------------------


ui <- navbarPage(theme = shinytheme("united"),
                 title = "shiny plots",
                 
                 tabPanel("data",
                          icon  = icon("tasks"),
                          data_upload
                 ),
                 
                 tabPanel("ggplot",
                          icon  = icon("binoculars"),
                          ggplot
                 ),
                 tabPanel("readme",
                          icon  = icon("info-circle"),
                          readme
                 )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(df = ggplot2::diamonds)
  
  observeEvent(input$file1$datapath, {
    
    if (input$data_use == 1) {
      values$df <- read.csv(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote,stringsAsFactors = F) }
  })
  
  code <- reactive({
    input$filter_code
  })
  
  observeEvent(input$update_data, {
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,stringsAsFactors = F) 
    if (input$filter_code == "") {
      if ( class(data[[input$filter_column]]) %in% c("numeric","integer")) {
        req(input$filter1)
        values$df <- data %>% 
          filter(eval(as.name(input$filter_column)) >= min(input$filter1),
                 eval(as.name(input$filter_column)) <= max(input$filter1))
      } else if (class(data[[input$filter_column]]) %in% c("character","factor") ){
        req(input$filter1)
        values$df <- data %>% 
          filter(eval(as.name(input$filter_column)) == input$filter1)
      } else {
        
        values$df <- data  
        
      } } else {
        values$df <-eval(parse(text = code()) )
      }
  }
  )
  
  
  output$contents <- renderDataTable({
    
    if(input$disp == "head") {
      return(head(values$df))
    }
    else {
      return(values$df)
    }
    
  })
  
  
  # ggplot input ------------------------------------------------------------------
  
  output$ggplot_input <- renderUI({
    
    div(  
      selectInput("x_ggplot", "X variable", choices = c(names(values$df))),
      selectInput("y_ggplot", "Y variable", choices = c(names(values$df))),
      selectInput("fill_ggplot", "Fill/Color", choices = c(names(values$df),"no_fill"),selected = "no_fill"),
      selectInput("facet", "Facet by", choices = c("",names(values$df)),selected = "",multiple = T),
    )    
    
  })
  # ggplot render ------------------------------------------------------------------  
  
  plot_geom <- reactive({
    
    if(input$fill_ggplot == "no_fill") {
      switch(input$type_ggplot,
             Line = geom_line(),
             Bar = geom_col(),
             Box = geom_boxplot(),
             Scatter = geom_point())
    } else {
      switch(input$type_ggplot,
             Line = geom_line(aes(color= .data[[input$fill_ggplot]])),
             Bar = geom_col(aes(fill= .data[[input$fill_ggplot]])),
             Box = geom_boxplot(aes(color= .data[[input$fill_ggplot]])),
             Scatter = geom_point(aes(color= .data[[input$fill_ggplot]]))) 
      
    }
  })
  
  plot_facet <- reactive({
    if(is.null(input$facet)) {
           NULL 
      } else { 
    facet_grid(~.data[[input$facet]])
}
  })
  
  plot_labs <- reactive({
    labs(x = input$x_ggplot, y = input$y_ggplot, 
         title = input$title_ggplot,
         subtitle = input$subtitle_ggplot,
         caption = input$caption_ggplot) 
  })
  
  plot_theme <- reactive({
    switch(input$theme,
           "default" = NULL,
           "wsj" = theme_wsj(),
           "excel" = theme_excel(),
           "economist" = theme_economist()
    ) 
  })
  
  data_a <- reactiveValues(annotations = data.frame(x = numeric(0), y = numeric(0),
                                                  color = character(0),label = character(0)))
  
  plot_annotation <- reactive({
    switch(input$annotate,
           "No" = NULL,
           "Yes" = if (nrow(data_a$annotations) > 0) {geom_text(data = data_a$annotations, aes(x,y,label = label, color = color), vjust = -1)} else {NULL}
    ) 
  })
  
  observeEvent(input$plot_click, {
    showModal(modalDialog(
      title = "Add annotation?",
      textInput(inputId = "annotation","Annotation",value = ""),
      selectInput(inputId = "anno_color","Choose a color",choices = c("blue","lightblue","yellow","lightyellow","black"),selected = "black"),
      footer = tagList(
        modalButton("No"),
        actionButton("yes", "Yes")
      )
    ))
  })
  
  observeEvent(input$yes, {
    data_a$annotations <- rbind(data_a$annotations, data.frame(x = input$plot_click$x, 
                                                           y = input$plot_click$y,
                                                           color = input$anno_color,
                                                           label = input$annotation))
    
    removeModal()
  })
  

  library_code <- "library(ggplot2)\n"
  
  # background impact 
  
  observeEvent(input$file_background$datapath, {
    
     library_code <- paste0(library_code,"library(ggimage)\n")
      
 })
  
  # logo impact 
  
  observeEvent(input$file_logo$datapath, {
    
    library_code <- library_code %>% 
      paste0("library(grid)\n") %>% 
      paste0("library(png)\n") 
    
  })
  
  
  output$plot <- renderPlot({
    
    df <- values$df  
    df2 <<- data_a$annotations
    req(input$fill_ggplot)
    
    p <- ggplot(df,aes(.data[[input$x_ggplot]], .data[[input$y_ggplot]])) + 
      plot_geom() +
      plot_facet() +
      plot_labs() + 
      plot_theme()
    
    if (nrow(data_a$annotations) > 0) {
      
    p <- p + geom_text(df2, mapping = aes(x,y, label = label), color = df2$color, vjust = -1)
      
    } else {
    p <- p
    }
     
    if (!is.null(input$file_background$datapath)) {
      p <- ggimage::ggbackground(p,background = input$file_background$datapath)
    } else { 
      p <- p
    }
    
    if (!is.null(input$file_logo$datapath)) {
      find_posi <- function(p) {
        y_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range
        x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
        return(c(xmin = (x_range[2] - x_range[1])*.8+x_range[1],
                 xmax = x_range[2],
                 ymax = y_range[1] - (y_range[2] - y_range[1])*.1, 
                 ymin = y_range[1] - (y_range[2] - y_range[1])*.3))
      }
      logo <- png::readPNG(input$file_logo$datapath) %>% 
    rasterGrob(width = .8,height = 1)
      
      position <- find_posi(p)
      
      p <- p + 
          annotation_custom(grob = logo,
                            xmin = position["xmin"], 
                            xmax = position["xmax"],
                            ymin = position["ymin"],
                            ymax = position["ymax"]
          ) +
          coord_cartesian(clip = "off") +
          theme(plot.margin = unit(c(1, 1, 3, 1), "lines")) 
    } else { 
      p <- p
    }
    
    p
  })
  
  final_code <- reactive({

    import_code <- if (input$data_use == 1) { "df <- ggplot2::diamonds"} else {
      glue::glue("df <- read.csv('{your_file_path}',
                                         header = '{input$header}',
                                         sep = '{input$sep}',
                                         quote = '{input$quote}',stringsAsFactors = F)")
                                         }

    gg_code <- paste0("ggplot(df,aes(",input$x_ggplot,", ",input$y_ggplot,"))")
    
    plot_geom_code <- if(input$fill_ggplot == "no_fill") {
      switch(input$type_ggplot,
             Line = "geom_line()",
             Bar = 'geom_col()',
             Box = 'geom_boxplot()',
             Scatter = 'geom_point()')
    } else {
      switch(input$type_ggplot,
             Line = paste0('geom_line(aes(color=',input$fill_ggplot,'))'),
             Bar = paste0('geom_col(aes(fill=',input$fill_ggplot,'))'),
             Box = paste0('geom_boxplot(aes(color= ',input$fill_ggplot,'))'),
             Scatter = paste0('geom_point(aes(color= ',input$fill_ggplot,'))'))
    }  
    
    
    plot_facet_code <- if(is.null(input$facet)) {
                              ''} else {
                              paste0('+ \nfacet_grid(~',input$facet,")") }
    
    plot_labs_code <- paste0('labs(x = ',"'",input$x_ggplot,"'",
                             ',', ' y = ',"'",input$y_ggplot,"'",
                             ',', ' title = ',"'",input$title_ggplot,"'",
                             ',', ' subtitle = ',"'",input$subtitle_ggplot,"'",
                             ',', ' caption = ',"'",input$caption_ggplot,"'",
                             ')') 
    
    plot_theme_code <- switch(input$theme,
                              "default" = '',
                              "wsj" = '+ \ntheme_wsj()',
                              "excel" = '+ \ntheme_excel()',
                              "economist" = '+ \ntheme_economist()'
    )
    
    plot_annotation_code <- switch(input$annotate,
                              "No" = '',
                              "Yes" = if (nrow(data_a$annotations) > 0) {"+ \ngeom_text(data = annotation_data, aes(x,y,label = label, color = color), vjust = -1)"} else {""}
    )
    
    plot_annotation_data_code <- switch(input$annotate,
                                        "No" = '',
                                        "Yes" = if (nrow(data_a$annotations) > 0) {
                                          paste0("\n","annotation_data <- ",
                                          dput(data_a$annotations) %>% 
                                            capture.output() %>% 
                                            paste0(collapse = ""),"\n\n")} else {""}
    )
    
    background_code1 <- if (!is.null(input$file_background$datapath)|!is.null(input$file_logo$datapath)) {
      paste0("p <- ")
    } else {""} 
    
    background_code2 <- if (!is.null(input$file_background$datapath)) {
      paste0("\n\n",glue::glue("ggbackground(p, background = '{'your_background_path'}')"),"\n")
    } else {""}  
    
    logo_code <- if (!is.null(input$file_logo$datapath)) {
      paste0("\n\n",glue::glue('logo <- png::readPNG({"your_logo_path"}) %>% 
        gird::rasterGrob(width = 1,height = 1)
        
        position <- find_posi(p)
        
        p <- p + 
        annotation_custom(grob = logo,
                          xmin = position["xmin"], 
                          xmax = position["xmax"],
                          ymin = position["ymin"],
                          ymax = position["ymax"]
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))')) 
      
      
    } else {""}  
      
   paste0(library_code,"\n", 
          import_code ,"\n\n",
          plot_annotation_data_code, 
          background_code1, gg_code, " + \n", 
          plot_geom_code," + \n", 
          plot_labs_code, 
          plot_facet_code, 
          plot_theme_code, 
          plot_annotation_code, 
          logo_code,
          background_code2)
    
  })
  
  # ggplot code
  
  output$text <- renderText({
    
    df <- values$df
    req(input$fill_ggplot)
    paste(final_code())
    
  })
  
  observeEvent(input$copy_gg,{
    clipr::write_clip(final_code())
    showNotification("Code copied!", type = "message",duration = 3)
  })
  
  # ggplot download 
  
  output$gg_download <- downloadHandler(
    filename = function() {
      paste("shiny_plot_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      
      ggsave(file,device = "png")
    }
  )
}

shinyApp(ui, server)
