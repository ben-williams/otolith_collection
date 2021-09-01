#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(lubridate)
library(scico)
library(funcr)
theme_set(theme_report())
library(shiny)

dat3 <- read.csv(here::here("output", "dat1.csv")) |>
    dplyr::select(-X)
dat4 <- read.csv(here::here("output", "dat2.csv")) |>
    dplyr::select(-X)

dat3 |>
    left_join(dat4) |>
    rename_all(tolower) |>
    mutate(species = case_when(species == 303 ~ "nork",
                               species == 330 ~ "dusk",
                               species == 301 ~ "pop",
                               species %in% c(307, 357) ~ "rebs")) |>
    group_by(year, species, haul_join, haul) |>
    tally() |>
    group_by(year, species) |>
    summarise(three = ifelse(n>=3, 1, 0),
              two = ifelse(n==2, 1, 0),
              one = ifelse(n==1, 1, 0)) |>
    mutate(threep = sum(three) / n(),
           twop = sum(two) / n(),
           onep = sum(one) / n()) |>
    group_by(species, year) |>
    mutate(sthree = sum(three),
           stwo = sum(two),
           sone = sum(one),
           n = n()) -> ddat

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Otolith samples"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("pop3",
                        "pop3:",
                        min = 0,
                        max = 5,
                        value = 3),
            sliderInput("pop2",
                        "pop2:",
                        min = 0,
                        max = 5,
                        value = 2),
            sliderInput("pop1",
                        "pop1:",
                        min = 0,
                        max = 5,
                        value = 1),
            sliderInput("dusk3",
                        "dusk3:",
                        min = 0,
                        max = 5,
                        value = 3),
            sliderInput("dusk2",
                        "dusk2:",
                        min = 0,
                        max = 5,
                        value = 2),
            sliderInput("dusk1",
                        "dusk1:",
                        min = 0,
                        max = 5,
                        value = 1),
            sliderInput("nork3",
                        "nork3:",
                        min = 0,
                        max = 5,
                        value = 3),
            sliderInput("nork2",
                        "nork2:",
                        min = 0,
                        max = 5,
                        value = 2),
            sliderInput("nork1",
                        "nork1:",
                        min = 0,
                        max = 5,
                        value = 1),
            sliderInput("rebs3",
                        "rebs3:",
                        min = 0,
                        max = 5,
                        value = 3),
            sliderInput("rebs2",
                        "rebs2:",
                        min = 0,
                        max = 5,
                        value = 2),
            sliderInput("rebs1",
                        "rebs1:",
                        min = 0,
                        max = 5,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {



    output$distPlot <- renderPlot({
        ddat |>
            group_by(species, year) |>
            summarise(hauls = mean(n),
                      three = mean(sthree),
                      two = mean(stwo),
                      one = mean(sone)) |>
            mutate(pop = ifelse(species == "pop", three * input$pop3 + two * input$pop2 + one * input$pop1, NA),
                   dusk = ifelse(species == "dusk", three * input$dusk3 + two * input$dusk2 + one * input$dusk1, NA),
                   nork = ifelse(species == "nork", three * input$nork3 + two * input$nork2 + one * input$nork1, NA),
                   rebs = ifelse(species == "rebs", three * input$rebs3 + two * input$rebs2 + one * input$rebs1, NA)) |>
            pivot_longer(c(pop, dusk, nork, rebs), names_to = "sample_type") |>
            drop_na(value) |>
            ggplot(aes(year, value)) +
            geom_line() +
            facet_wrap(~species, scales = "free_y") +
            geom_hline(yintercept = c(1000, 650), lty = 3) +
            scale_x_continuous(breaks = seq(2008,2018, 2)) +
            # scale_color_scico_d(palette = "roma") +
            expand_limits(y = 0)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
