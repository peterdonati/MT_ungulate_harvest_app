#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Shiny app for MT ungulate harvest
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(shinythemes)
library(tidyr)

h_dat <- read.csv("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/harvest_dat.csv")
h_dat$District <- as.character(h_dat$District)

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
             
             # Home ============================================================
             tabPanel("Home",
                      h3(strong("Welcome!")),
                      div(
                        "This app allows you to explore past hunter harvest and 
                        effort estimates made by Montana FWP since 2004.", 
                        br(), 
                        br(),
                        "Navigating to the", strong("'District Visuals'"), "tab 
                        above allows you to create visualizations for specific 
                        hunting districts.",
                        br(),
                        "Navigating to the", strong("'Summary Tables'"), "tab 
                        allows you to search, arrange, and download a .csv 
                        based on the criteria you enter."
                      )
             ),
             
             # District Visuals ================================================
             tabPanel("District Visuals",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(
                            "ung_sp",
                            "Species",
                            choices = c(
                              "Deer" = "deer", 
                              "Elk" = "elk"
                            )
                          ),
                          
                          uiOutput("deer_sp"),
                          
                          sliderInput(
                            inputId = "yr",
                            label = "Years",
                            min = 2004,
                            max = 2019,
                            value = c(2004, 2019),
                            step = 1,
                            ticks = FALSE,
                            sep = ""
                          ),
                          
                          selectInput(
                            inputId = "dist",
                            label = "Hunting District",
                            choices = NULL,
                            multiple = TRUE
                          ),
                          
                          selectInput(
                            "yaxis",
                            "Y axis",
                            choices = c(
                              "Harvest" = "T_harvest",
                              "Hunter effort" = "Hunters",
                              "Success rate" = "p_success"
                            )
                          ),
                          
                          uiOutput("split"),
                          sliderInput("height", "Plot height", 
                                      min = 300, 
                                      max = 1000, 
                                      value = 400),
                          
                          verbatimTextOutput("nodata")
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "plot")
                        )
                      )
             ),
             
             # Summary Tables ==================================================
             tabPanel("Summary Tables",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(
                            inputId = "ung_sp_2",
                            label = "Species",
                            choices = c("Deer" = "deer", 
                                        "Elk" = "elk")
                          ),
                          
                          uiOutput("deer_sp_2"),
                          
                          sliderInput(
                            inputId = "yr_2",
                            label = "Years",
                            min = 2004,
                            max = 2019,
                            value = c(2004, 2019),
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
                            choices = c(
                              "Total harvest" = "T_harvest",
                              "Hunter effort" = "Hunters",
                              "Success rate" = "p_success",
                              "Category and category harvest" = "group"
                            )
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
                      HTML("<p>Send me your errors, suggestions, or questions!
                             </br>p.donati11@gmail.com</p>")
             )
  )
)

# server =======================================================================
server <- function(input, output, session){
  
  # District Visuals tab =======================================================
  # UI for split data (FWP doesn't publish weapon type for antelope)
  output$split <- renderUI({
    if(input$yaxis == "T_harvest"){
      selectInput(
        inputId = "split",
        label = "Split",
        choices = c("Nothing", "Weapon", "Sex")
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
    text = element_text(family = "serif", size = 16),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.key = element_blank()
  )
  
  # Plot creation:
  output$plot <- renderPlot(
    width = 700,
    height = function() input$height,
    {
      # Grab the years to plot:
      years <- input$yr[[1]]:input$yr[[2]]
      
      #Generate a new dataset containing only selected variables:
      plot_dat <- filter(h_dat, ung == input$ung_sp)
      plot_dat <- plot_dat[which(plot_dat$Year %in% years), ]
      plot_dat <- plot_dat[which(plot_dat$District %in% input$dist), ]
      if (input$ung_sp == "deer"){
        plot_dat <- filter(plot_dat, Species == input$deer_sp)
      }
      
      if (!is.null(input$dist)){
        # Harvest plot ----
        if (input$yaxis == "T_harvest"){
          # GRAPH FOR EFFORT OR NO SPLIT:
          if (input$split == "Nothing" || is.null(input$split)){
            
            return(
              ggplot(plot_dat, aes(x = Year, y = T_harvest, color = District)) +
                geom_line() +
                geom_point() +
                labs(
                  y = "Estimated harvest"
                ) +
                scale_x_continuous(minor_breaks = NULL) +
                mytheme
            )
          } else {
            
            # GRAPH FOR SPLITS:
            # Defining y variable and split:
            if (input$split == "Sex") { 
              plot_dat <- filter(
                plot_dat, 
                group %in% c("Bucks", "Does", "Bulls", "Cows")
              )
            } else if (input$split == "Weapon") {
              plot_dat <- filter(
                plot_dat, 
                group %in% c("Rifle", "Bow")
              )
            } 

            return(
              ggplot(plot_dat, aes(x = Year, y = group_harvest)) +
                geom_line(aes(linetype = group)) +
                geom_point() + 
                facet_wrap(~District, ncol = 2) +
                labs(
                  y = "Estimated harvest",
                  linetype = input$split
                ) +
                scale_x_continuous(minor_breaks = NULL) +
                mytheme
            )
          }
        } else if(input$yaxis == "Hunters"){
          # Hunter effort plot ----
          plot_dat <- plot_dat[which(!is.na(plot_dat$Hunters)), ]
          
          return(
            ggplot(plot_dat, aes(x = Year, y = Hunters, color = District)) +
              geom_line(na.rm = TRUE) +
              geom_point(na.rm = TRUE) +
              labs(
                y = "# of hunters per season"
              ) +
              scale_x_continuous(minor_breaks = NULL) +
              mytheme
          )
        } else if (input$yaxis == "p_success"){
          # % success plot ----
          plot_dat <- plot_dat[which(!is.na(plot_dat$Hunters)), ]
          
          return(
            ggplot(plot_dat, aes(x = Year, y = p_success, color = District)) +
              geom_line(na.rm = TRUE) +
              geom_point(na.rm = TRUE) +
              labs(
                y = "Success rate per hunter"
              ) +
              scale_x_continuous(minor_breaks = NULL) +
              scale_y_continuous(labels = scales::percent_format()) +
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
            panel.border = element_blank()
          )
      }
    })
  
  # when data is missing for selection:
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
  
  # Summary Table tab ==========================================================
  
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

    if (input$ung_sp_2 == "deer" & !is.null(input$deer_sp_2)){
      targ_sp <- filter(targ_sp, Species == input$deer_sp_2)
    }

    targ_sp <- targ_sp %>%
      group_by(District) %>%
      summarise(mean_harvest = mean(T_harvest), .groups = "drop")

    updateSliderInput(session, "avg_hvst",
                      min = round(min(targ_sp$mean_harvest), 0) - 1,
                      max = round(max(targ_sp$mean_harvest), 0) + 1,
                      value = c((min(targ_sp$mean_harvest)) - 1,
                                (max(targ_sp$mean_harvest) +1))
    )
  })

  # Creation of table ==========================================================
  user_dat <- reactive({
    yrs_2 <- input$yr_2[[1]]:input$yr_2[[2]]
    
    dat <- filter(h_dat, ung == input$ung_sp_2)
    dat <- dat[which(dat$Year %in% yrs_2), ]
    
    # Had to add !is.null. because following evaluates before deer_sp_2 has a
    # value, and then throws an error briefly for when it tries to filter by it
    if (input$ung_sp_2 == "deer" & !is.null(input$deer_sp_2)){
      dat <- filter(dat, Species == input$deer_sp_2)
    }
    
    dat <- dat %>%
      group_by(group, District) %>%
      summarise(
        Yr_start = input$yr_2[[1]],
        Yr_end = input$yr_2[[2]],
        deer_sp = Species[[1]],
        Hunters = mean(Hunters, na.rm = TRUE),
        T_harvest = mean(T_harvest),
        group_harvest = mean(group_harvest),
        p_success = mean(p_success, na.rm = TRUE),
        .groups = "drop"
      )
    dat <- select(dat, District, Yr_start, Yr_end, deer_sp, Hunters, 
                  T_harvest, group, group_harvest, p_success)
    
    if (!is.null(input$avg_hvst)){
      dat <- filter(dat, T_harvest >= input$avg_hvst[[1]] 
                    & T_harvest <= input$avg_hvst[[2]])
    }
    
    if (input$arrange != "group"){
      targ_arrange <- sym(input$arrange)
      if (input$descend == FALSE){
        dat <- arrange(dat, !!targ_arrange)
      } else if (input$descend == TRUE){
        dat <- arrange(dat, desc(!!targ_arrange))
      } 
    } else if (input$arrange == "group"){
      if (input$descend == FALSE){
        dat <- arrange(dat, group, group_harvest)
      } else if (input$descend == TRUE){
        dat <- arrange(dat, group, desc(group_harvest))
      }
    }
    
    names(dat) <- c("District", "From", "To", "Deer species",
                    "Avg. # of hunters per season",
                    "Avg. total harvest", "Category",
                    "Avg. harvest for category",
                    "Avg. Proportion of hunters\n that are successful")
    if (input$ung_sp_2 == "elk"){
      dat <- dat[ ,-4]
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
