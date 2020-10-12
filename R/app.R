################################################################################
#
# Shiny app for MT ungulate harvest
#
################################################################################
library(shiny)
library(RCurl)
library(dplyr)
library(ggplot2)
library(rlang)
library(shinythemes)

#h_dat <- read.csv(text = getURL("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/harvest_dat.csv"))
h_dat <- read.csv("C:/Users/Peter/Documents/GitHub/MT_ungulate_harvest_app/Datasets/harvest_dat.csv")
h_dat$District <- as.character(h_dat$District)
################################################################################

# UI ===========================================================================
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Montana ungulate harvest estimates by district"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "ung_sp",
        label = "Species",
        choices = c("Deer" = "deer", 
                    "Elk" = "elk",
                    "Antelope" = "ant")
      ),
      sliderInput(
        inputId = "yr",
        label = "Years",
        min = 2010,
        max = 2019,
        value = c(2010, 2019),
        step = 1,
        ticks = FALSE,
        sep = ""
      ),
      selectInput(
        inputId = "dist",
        label = "Districts",
        choices = NULL,
        multiple = TRUE
      ),
      uiOutput("split"),
      uiOutput("deer_sp")
    ),
    
    mainPanel(
      plotOutput(outputId = "plot", height = 700, width = 1000),
      textOutput("nodata")
    )
  )
)

# server =======================================================================
server <- function(input, output, session){
  
  # UI for split data (FWP doesn't publish weapon type for antelope)
  output$split <- renderUI({
    if(input$ung_sp != "ant")
      selectInput(
        inputId = "split",
        label = "Split",
        choices = c("Nothing", "Weapon", "Sex")
      ) else if (input$ung_sp == "ant"){
        selectInput(
          inputId = "split",
          label = "Split",
          choices = c("Nothing", "Sex")
        )
      }
  })
    
  # Need to show this UI if looking at deer harvest:
  output$deer_sp <- renderUI({
    if (input$ung_sp == "deer"){
      selectInput(
        inputId = "deer_sp",
        label = "Deer species",
        choices = c("Combined" = "all_deer",
                    "Whitetail" = "wt", 
                    "Mule" = "md")
      )
    }
  })
  
  # Update input$dist choices according to species:
  district_choice <- reactive({
    filter(h_dat, ung == input$ung_sp)
  })
  observeEvent(district_choice(), {
    updateSelectInput(session, "dist",
                      choices = unique(district_choice()$District))
  })
  
  mytheme <- theme(
    text = element_text(family = "serif", size = 18),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.key = element_rect(fill = NA)
  )
  
  # Plot creation:
  output$plot <- renderPlot({
    
    # Later used in scale_x_continuous() to standardize x axis for when
    # districts are missing data for some years:
    years <- input$yr[[1]]:input$yr[[2]]
    
      #Generate a new dataset containing only selected variables:
      plot_dat <- filter(h_dat, ung == input$ung_sp)
      plot_dat <- plot_dat[which(plot_dat$Year %in% years), ]
      plot_dat <- plot_dat[which(plot_dat$District %in% input$dist), ]
      if (input$ung_sp == "deer"){
        plot_dat <- filter(plot_dat, Species == input$deer_sp)
      }
    
    if (!is.null(input$dist)){
      
      # GRAPH FOR EFFORT OR NO SPLIT:
      if (input$split == "Nothing" || is.null(input$split)){
        
        return(
          ggplot(plot_dat, aes(x = Year, y = N, color = District)) +
            geom_line() +
            geom_point(size = 2) +
            labs(
              y = "Estimated harvest"
            ) +
            scale_x_continuous(breaks = years, minor_breaks = F) +
            mytheme
        )
      } else {
        
        # GRAPH FOR SPLITS:
        # Defining y variable and split:
        if (input$split == "Sex") { 
          y_var <- sym("n_sex")
        } else if (input$split == "Weapon") {
          y_var <- sym("n_weapon")
        } 
        split <- sym(input$split)
        
        return(
          ggplot(plot_dat, aes(x = Year, y = !!y_var)) +
            geom_line(aes(linetype = !!split)) +
            geom_point(size = 2) + 
            facet_wrap(~District, ncol = 2) +
            labs(
              y = "Estimated harvest"
            ) +
            scale_x_continuous(breaks = years, minor_breaks = F) +
            mytheme
        )
      }
    } else {
      # Placeholder for when no district is chosen:
      ggplot() +
        annotate("text", 
                 label = "Plot created once district chosen", 
                 x = 0, y= 0, 
                 size = 7,
                 family = "serif") +
        theme_void() +
        theme(
          panel.border = element_rect(fill = NA, color = "black")
        )
    }
  })

  
  # NEED TO FIGURE THIS OUT FOR CASES WHEN DATA IS MISSING IN YEARS
  # output$nodata <- renderText({
  #   plot_dat <- filter(h_dat, ung == input$ung_sp)
  #   plot_dat <- plot_dat[which(plot_dat$Year %in% years), ]
  #   plot_dat <- plot_dat[which(plot_dat$District %in% input$dist), ]
  #   if (input$ung_sp == "deer"){
  #     plot_dat <- filter(plot_dat, Species == input$deer_sp)
  #   }
  #   
  #   if (!all(input$dist %in% plot_dat$District)){
  #   paste("No data for district",
  #         input$dist[[which(!input$dist %in% plot_dat$District)]],
  #         "for selected years")
  #   } else {""}
  # })

}


# The app ======================================================================
shinyApp(ui, server)
