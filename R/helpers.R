# Helper functions/objects
# ~~~~~~~~~~~~~~~~~~~~~~~~

# UI creation for similar "deer" and "elk" tabs ================================
plot_ui <- function(prefix, dataset){
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        inputId = paste0(prefix, "_dist"),
        label = "Hunting District(s)",
        choices = unique(dataset$hunting_district),
        multiple = TRUE
      ),
      sliderInput(
        inputId = paste0(prefix, "_yr"),
        label = "Years",
        min = 2004,
        max = 2020,
        value = c(2004, 2020),
        step = 1,
        ticks = FALSE,
        sep = ""
      ),
      if (prefix == "d"){
        selectInput(
          inputId = "d_sp",
          label = "Deer species",
          choices = c(
            "Combined" = "all_deer",
            "Whitetail" = "wt", 
            "Mule" = "md"
          )
        )
      },
      selectInput(
        inputId = paste0(prefix, "_yaxis"), 
        label = "Y axis", 
        choices = c(
          "Harvest" = "total_harvest",
          "Hunters" = "hunters",
          "Success rate" = "success_rate_per_hunter"
        )
      ),
      uiOutput(paste0(prefix, "_split")),
      sliderInput(
        inputId = paste0(prefix, "_height"), 
        label = "Plot height", 
        min = 300, 
        max = 1000, 
        value = 400
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Visual", plotOutput(paste0(prefix, "_plot"))),
        tabPanel(
          "Table",
          div(
            DTOutput(paste0(prefix, "_table")), 
            style = "font-family:Arial, Helvetica, serif; font-size:12px"
          )
        )
      )
    )
  )
}

# Function to avoid weird breaks on plots ======================================
good_breaks <- function(x){
  x[1] <- trunc(x[1]) + 1
  x[2] <- trunc(x[2])
  years <- x[1]:x[2]
  
  if (length(years) <= 6){
    out <- (years)
  } else if (length(years) <= 12){
    out <- seq(max(years), min(years), by = -2)
  } else if (length(years) > 12){
    out <- (seq(max(years), min(years), by = -3))
  }
  out <- sort(out)
  return(out)
}
# Plot theme ===================================================================
mytheme <- theme(
  text = element_text(family = "sans", size = 12),
  panel.background = element_blank(),
  panel.border = element_rect(fill = NA, color = "black"),
  legend.key = element_blank()
)

# Plot creations ===============================================================

# ~~~~~~~~
plot_total_harvest <- function(this_dat){
  ggplot(
    this_dat, 
    aes(x = license_year, y = total_harvest, color = hunting_district)
  ) +
    geom_line() +
    labs(
      x = "Year",
      y = "Estimated harvest",
      color = "District"
    ) +
    scale_x_continuous(breaks = good_breaks) +
    mytheme
}

# ~~~~~~~~
plot_sex_harvest <- function(this_dat){
  ggplot(this_dat, aes(x = license_year, y = s_harv)) +
    geom_line(aes(linetype = sex)) +
    facet_wrap(~hunting_district, ncol = 2) +
    labs(x = "Year", y = "Estimated harvest", linetype = "Sex") +
    scale_x_continuous(minor_breaks = FALSE) +
    mytheme
}

# ~~~~~~~~
plot_weapon_harvest <- function(this_dat){
  ggplot(this_dat, aes(x = license_year, y = w_harv)) +
    geom_line(aes(linetype = weapon)) +
    facet_wrap(~hunting_district, ncol = 2) +
    labs(x = "Year", y = "Estimated harvest", linetype = "Weapon") +
    scale_x_continuous(breaks = good_breaks) +
    mytheme
}

# ~~~~~~~~
plot_hunters <- function(this_dat){
  ggplot(this_dat, 
         aes(x = license_year, y = hunters, color = hunting_district)) +
  geom_line() +
  geom_point(size = 1) +
  labs(
    x = "Year",
    y = "# of hunters per season",
    color = "District"
  ) +
  scale_x_continuous(breaks = good_breaks) +
  mytheme
}

# ~~~~~~~~
plot_success <- function(this_dat){
  ggplot(
    this_dat, 
    aes(x = license_year, y = success_rate_per_hunter, color = hunting_district)
  ) +
    geom_line() +
    geom_point(size = 1) +
    labs(
      x = "Year",
      y = "Percent success per hunter",
      color = "District"
    ) +
    scale_x_continuous(breaks = good_breaks) +
    scale_y_continuous(labels = scales::percent_format()) +
    mytheme
} 

# ~~~~~~~~  
plot_empty <- function(){
  ggplot() +
    annotate(
      "text", 
      label = "Plot created once district chosen", 
      x = 0, y = 0, 
      size = 7,
      family = "serif"
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = "light grey", color = NA))
}
