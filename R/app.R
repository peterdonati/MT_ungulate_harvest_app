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
library(tidyr)

h_dat <- read.csv(text = getURL("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/harvest_dat.csv"))
h_dat$District <- as.character(h_dat$District)
################################################################################

# UI ===========================================================================
ui <- fluidPage(
  
  # Google analytics ===========================================================
  tags$head(HTML(
    "<!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-180466253-1'></script>
    <script>
    window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  
  gtag('config', 'UA-180466253-1');
  </script>"
  )),
  
  theme = shinytheme("yeti"),
  
  navbarPage("Montana ungulate harvest",
             # Search by district ==============================================
             tabPanel("Search by district",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "ung_sp",
                            label = "Species",
                            choices = c("Deer" = "deer", 
                                        "Elk" = "elk",
                                        "Antelope" = "ant")
                          ),
                          uiOutput("deer_sp"),
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
                          sliderInput("height", "Plot height", 
                                      min = 500, 
                                      max = 1000, 
                                      value = 600),
                          verbatimTextOutput("nodata")
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "plot")
                        )
                      )
             ),
             
             # Search by criteria ==============================================
             tabPanel("Search by criteria",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "ung_sp_2",
                            label = "Species",
                            choices = c("Deer" = "deer", 
                                        "Elk" = "elk",
                                        "Antelope" = "ant")
                          ),
                          uiOutput("deer_sp_2"),
                          sliderInput(
                            inputId = "yr_2",
                            label = "Years",
                            min = 2010,
                            max = 2019,
                            value = c(2010, 2019),
                            step = 1,
                            ticks = FALSE,
                            sep = ""
                          ),
                          sliderInput(
                            "avg_hvst",
                            "Average total harvest",
                            min = 0,
                            max = 1,
                            value = c(0,1),
                            step = 1,
                            ticks = F,
                            sep = ""
                          ),
                          selectInput(
                            "arrange",
                            "Arrange by",
                            choices = NULL
                          ),
                          checkboxInput(
                            "descend",
                            "Descending?",
                            TRUE
                          ),
                          downloadButton("download", "Download .csv")
                        ),
                        
                        mainPanel(
                          tableOutput("filtered_hvst_dat")
                        )
                      )
             ),
             
             # About ===========================================================
             tabPanel("About",
                        tags$h1("About the project"),
                      tags$div(
                        "All raw data on past harvest estimates comes",
                        tags$a(href="https://myfwp.mt.gov/fwpPub/harvestReports", 
                               "from FWP")
                        ),
                        tags$div(
                          "The exact data and source code used in this app can be accessed",
                          tags$a(href="https://github.com/peterdonati/MT_ungulate_harvest_app", 
                                 "here at my GitHub")
                        ),
                      tags$h1("Contact"),
                        HTML("<p>Any errors, suggestions, or questions can be 
                             sent to p.donati11@gmail.com</p>")
                      
             )
  )
)
  
  
# server =======================================================================
server <- function(input, output, session){
  
  # SEARCH BY DISTRICT TAB ======================================================
  # UI for split data (FWP doesn't publish weapon type for antelope)
  output$split <- renderUI({
    if (input$ung_sp != "ant"){
      selectInput(
        inputId = "split",
        label = "Split",
        choices = c("Nothing", "Weapon", "Sex")
      )
    } else if (input$ung_sp == "ant"){
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
  output$plot <- renderPlot(
    width = 850,
    height = function() input$height,
    {
    
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
                 x = 0, y = 0, 
                 size = 7,
                 family = "serif") +
        theme_void() +
        theme(
          panel.border = element_rect(fill = NA, color = "black")
        )
    }
  })

  output$nodata <- renderText({
  yr_test <- input$yr[[1]]:input$yr[[2]]
  
  dist_test <- filter(h_dat, ung == input$ung_sp)
  dist_test <- dist_test[which(dist_test$Year %in% yr_test), ]
  
  if (!all(input$dist %in% dist_test$District)){
    paste0("No data for district ",
          paste(input$dist[which(!input$dist %in% dist_test$District)], 
                collapse = " or "),
          " for selected years\n")
  } else {""}
  })
  
  # SEARCH BY CRITERIA TAB =====================================================
  
  output$deer_sp_2 <- renderUI({
    if (input$ung_sp_2 == "deer"){
      selectInput(
        inputId = "deer_sp_2",
        label = "Deer species",
        choices = c("Combined" = "all_deer",
                    "Whitetail" = "wt", 
                    "Mule" = "md")
      )
    }
  })
  
  #Updating avg. harvest slider:
  observeEvent(c(input$ung_sp_2, input$yr_2, input$deer_sp_2), {
    
    yrs_2 <- input$yr_2[[1]]:input$yr_2[[2]]
    
    targ_sp <- filter(h_dat, ung == input$ung_sp_2)
    targ_sp <- targ_sp[which(targ_sp$Year %in% yrs_2), ]
    
    if (input$ung_sp_2 == "deer" && !is.null(input$deer_sp_2)){
      targ_sp <- filter(targ_sp, Species == input$deer_sp_2)
    }
    
    targ_sp <- targ_sp %>% 
      group_by(District,) %>% 
      summarise(mean_N = mean(N), .groups = "drop")
    
    updateSliderInput(session, "avg_hvst",
                      min = round(min(targ_sp$mean_N), 0) - 1,
                      max = round(max(targ_sp$mean_N), 0) + 1,
                      value = c((min(targ_sp$mean_N)) - 1, 
                                (max(targ_sp$mean_N) +1))
    )
  })
  
  # Updating arrange choices:
  observeEvent(input$ung_sp_2, {
    
    if (input$ung_sp_2 == "deer"){
      updateSelectInput(
        session, "arrange",
        choices = c("Avg. total harvest" = "N",
                    "Avg. buck harvest" = "n_buck",
                    "Avg 4+ pt buck harvest" = "n_four_plus",
                    "Avg. hunter effort" = "n_hunter")
      )
    } else if (input$ung_sp_2 == "elk"){
      updateSelectInput(
        session, "arrange",
        choices = c("Avg. total harvest" = "N",
                    "Avg. bull harvest" = "n_bull",
                    "Avg 6+ pt bull harvest" = "n_six_plus",
                    "Avg. hunter effort" = "n_hunter")
      )
    } else if (input$ung_sp_2 == "ant"){
      updateSelectInput(
        session, "arrange",
        choices = c("Avg. total harvest" = "N",
                    "Avg. buck harvest" = "n_buck",
                    "Avg. hunter effort" = "n_hunter")
      )
    }
  })
  
  #

  # Creation of table ==========================================================
  user_dat <- reactive({
    yrs_2 <- input$yr_2[[1]]:input$yr_2[[2]]
    
    dat <- filter(h_dat, ung == input$ung_sp_2)
    dat <- pivot_wider(
      dat, c(-Weapon, -n_weapon), names_from = Sex, values_from = n_sex
    )
    dat <- dat[which(dat$Year %in% yrs_2), ]
    # FWP puts 0's in years they don't estimate hunter effort......:
    dat$Hunter <- case_when(dat$Hunters == 0 ~ NA_integer_)
    
    # Had to add !is.null... because following evaluates before deer_sp_2 has a 
    # value, and then throws an error briefly for when it tries to filter by it
    if (input$ung_sp_2 == "deer"  && !is.null(input$deer_sp_2)){
      # Deer ----
      
      dat <- filter(dat, Species == input$deer_sp_2)
      
      dat <- dat %>% 
        group_by(District) %>% 
        summarise(
          Yr_start = input$yr_2[[1]],
          Yr_end = input$yr_2[[2]],
          deer_sp = Species[[1]],
          n_hunter = mean(Hunters, na.rm = TRUE),
          N = mean(N),
          n_buck = mean(Bucks),
          n_doe = mean(Does),
          n_four_plus = mean(four_plus),
          .groups = "drop"
        )
      
      if (!is.null(input$avg_hvst)){
        dat <- filter(dat, N >= input$avg_hvst[[1]] & N <= input$avg_hvst[[2]])
      }
      
      targ_arrange <- sym(input$arrange)
      
      if (input$descend == FALSE){
        dat <- arrange(dat, !!targ_arrange)
      } else if (input$descend == TRUE){
        dat <- arrange(dat, desc(!!targ_arrange))
      }
      
      names(dat) <- c("District", "From", "To", "Deer species", 
                      "Avg. number of hunters throughout season", 
                      "Avg. total harvest", "Avg. buck harvest", 
                      "Avg. doe harvest", "Avg. 4+ pt. buck harvest")
      
    } else if (input$ung_sp_2 == "elk"){
      
      # Elk ----
      dat <- dat %>% 
        group_by(District) %>% 
        summarise(
          Yr_start = input$yr_2[[1]],
          Yr_end = input$yr_2[[2]],
          n_hunter = mean(Hunters, na.rm = TRUE),
          N = mean(N),
          n_bull = mean(Bulls),
          n_cow = mean(Cows),
          n_six_plus = mean(six_plus),
          .groups = "drop"
        )
      
      if (!is.null(input$avg_hvst)){
        dat <- filter(dat, N >= input$avg_hvst[[1]] & N <= input$avg_hvst[[2]])
      }
      
      targ_arrange <- sym(input$arrange)
      
      if (input$descend == FALSE){
        dat <- arrange(dat, !!targ_arrange)
      } else if (input$descend == TRUE){
        dat <- arrange(dat, desc(!!targ_arrange))
      }
      
      names(dat) <- c("District", "From", "To", 
                      "Avg. number of hunters throughout season", 
                      "Avg. total harvest", "Avg. bull harvest", 
                      "Avg. cow harvest", "Avg. 6+ pt. bull harvest")
      
    } else if (input$ung_sp_2 == "ant"){
      
      # Antelope ----
      dat <- dat %>% 
        group_by(District) %>% 
        summarise(
          Yr_start = input$yr_2[[1]],
          Yr_end = input$yr_2[[2]],
          n_hunter = mean(Hunters, na.rm = TRUE),
          N = mean(N),
          n_buck = mean(Bucks),
          n_doe = mean(Does),
          .groups = "drop"
        )
      
      if (!is.null(input$avg_hvst)){
        dat <- filter(dat, N >= input$avg_hvst[[1]] & N <= input$avg_hvst[[2]])
      }
      
      targ_arrange <- sym(input$arrange)
      
      if (input$descend == FALSE){
        dat <- arrange(dat, !!targ_arrange)
      } else if (input$descend == TRUE){
        dat <- arrange(dat, desc(!!targ_arrange))
      }
      
      names(dat) <- c("District", "From", "To", 
                      "Avg. number of hunters throughout season", 
                      "Avg. total harvest", "Avg. buck harvest", 
                      "Avg. doe harvest")
      
    }
    return(dat)
  })
  
  # Output of table ----
  output$filtered_hvst_dat <- renderTable({user_dat()})
  
  # Download output ----
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$ung_sp_2, "_harvest", ".csv")
    },
    content = function(file) {
      write.csv(user_dat(), file, row.names = FALSE)
    }
  )
}

# The app ======================================================================
shinyApp(ui, server)
