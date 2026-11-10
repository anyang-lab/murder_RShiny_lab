# ============================================================
# 10 R Shiny
# ------------------------------------------------------------
# How to use
#   · Select each STEP's code block by dragging, then run it
#   · When the app window opens, try it out; stop it by closing the window
#     or with the red Stop button in the Console (or Esc)
#   · While the app is running, the Console is blocked → always stop it before the next STEP
#
# ============================================================


# ============================================================
# STEP 0. Setup
# ============================================================

# Install once (first time only)
# install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "dslabs"))
# install.packages(c("shiny", "bslib"))

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(dslabs)

data(murders)

# Check the data before building the app
head(murders)
unique(murders$region)        # values to use as choices in selectInput
range(murders$population)     # values to use as min/max in sliderInput


# ============================================================
# STEP 1. First app: a slider
# ------------------------------------------------------------
# What to check: moving the slider changes input$n → renderText re-runs
#                → output$result updates (no page refresh!)
# ============================================================

ui_1 <- page_sidebar(
  title = "My First Shiny App",
  sidebar = sidebar(
    sliderInput("n", "Pick a number:",
                min = 1, max = 100, value = 50)
  ),
  textOutput("result")
)

server_1 <- function(input, output) {
  output$result <- renderText({
    paste("The square of your number is:", input$n^2)
  })
}

shinyApp(ui_1, server_1)

# [Exercise 1-1] Change server_1 so that it shows the 'cube' instead of the square.
# [Exercise 1-2] Replace sliderInput with numericInput("n", "Enter a number:", value = 50).
#            Do you need to change the server code?




# ---- Exercise 1-1 ----------------------------------------------
# output$result <- renderText({
#   paste("The cube of your number is:", input$n^3)
# })

# ---- Exercise 1-2 ----------------------------------------------
# No change to the server is needed. As long as the inputId is the same ("n"),
# the server receives it as input$n, whatever the widget is.



# ============================================================
# STEP 2. Combined app
# ------------------------------------------------------------
# Two inputs (selected_region, pop_range)
#   → one reactive computation filtered_data()
#   → the plot (murder_plot) and the table (result) update together
# ============================================================

ui_2 <- page_sidebar(
  title = "Exploring US Murder Rates",
  sidebar = sidebar(
    selectInput("selected_region", "Region",
                choices = unique(murders$region),
                selected = "Northeast"),
    sliderInput("pop_range", "Population range",
                min = 0, max = max(murders$population),
                value = range(murders$population))
  ),
  plotOutput("murder_plot"),
  tableOutput("result")
)

server_2 <- function(input, output) {

  filtered_data <- reactive({
    murders %>%
      filter(region == input$selected_region,
             population >= input$pop_range[1],
             population <= input$pop_range[2])
  })

  output$murder_plot <- renderPlot({
    ggplot(filtered_data(), aes(population, total)) +
      geom_point(color = "#B57A23", size = 3) +
      labs(x = "Population", y = "Number of murders") +
      theme_minimal()
  })

  output$result <- renderTable({
    filtered_data()
  })
}

shinyApp(ui_2, server_2)

# Note
#   · When you change the region, do the plot and the table change 'together'?  → filtered_data() is reused
#   · What happens if you make the slider range very narrow?                  → empty plot (improved in STEP 4)
#
# [Exercise 2-1] Label each point with the state abbreviation (abb).
#            Hint: + geom_text(aes(label = abb), vjust = -1)
# [Exercise 2-2] Of course, states with more people have more murders.
#            Change the y-axis to the 'murder rate per 100,000 people'.
#            Hint: inside filtered_data(), mutate(rate = total / population * 1e5)



# ---- Exercise 2-1 ----------------------------------------------
#   ggplot(filtered_data(), aes(population, total)) +
#     geom_point(color = "#B57A23", size = 3) +
#     geom_text(aes(label = abb), vjust = -1) + ...

# ---- Exercise 2-2 ----------------------------------------------
#   filtered_data <- reactive({
#     murders %>%
#       mutate(rate = total / population * 1e5) %>%
#       filter(...)
#   })
#   ggplot(filtered_data(), aes(population, rate)) + ... +
#     labs(y = "Murders per 100,000")

# ============================================================
# STEP 3. Find the bugs: ID mismatches
# ------------------------------------------------------------
# The app below has '3' hidden errors. Run it, then find and fix them.
# Rules: inputId ↔ input$id, Output ID ↔ output$id, exact match including upper/lower case
#        *Output() in the UI, render*() in the server — they must be a matching pair
# ============================================================

ui_3 <- page_sidebar(
  title = "Find the Bugs",
  sidebar = sidebar(
    selectInput("selected_region", "Region",
                choices = unique(murders$region))
  ),
  plotOutput("murderPlot"),
  textOutput("n_states")
)

server_3 <- function(input, output) {

  filtered_data <- reactive({
    murders %>% filter(region == input$Selected_region)
  })

  output$murder_plot <- renderPlot({
    ggplot(filtered_data(), aes(population, total)) +
      geom_point() + theme_minimal()
  })

  output$n_states <- renderTable({
    paste("States selected:", nrow(filtered_data))
  })
}

shinyApp(ui_3, server_3)


# ---- STEP 3: the 3 bugs ---------------------------------------
# ① UI plotOutput("murderPlot")  ↔  server output$murder_plot   → ID mismatch
#    → use plotOutput("murder_plot") in both places
# ② input$Selected_region  (capital S)  → input$selected_region
# ③ renderTable paired with textOutput + missing parentheses on filtered_data
#    → output$n_states <- renderText({
#         paste("States selected:", nrow(filtered_data()))
#       })



# ============================================================
# STEP 4. More widgets and layout
# ------------------------------------------------------------
# New in this step
#   · radioButtons()  : choose exactly one → switch the y-axis variable
#   · checkboxInput() : TRUE/FALSE → turn the regression line on/off
#   · layout_columns(col_widths = c(7, 5)) : 12-column grid
#   · textOutput + renderText : a summary sentence
#   · validate(need(...)) : a message when the result is empty
# ============================================================

murders_r <- murders %>% mutate(rate = total / population * 1e5)

ui_4 <- page_sidebar(
  title = "Exploring US Murder Rates v2",
  sidebar = sidebar(
    selectInput("selected_region", "Region",
                choices = unique(murders_r$region)),
    sliderInput("pop_range", "Population range",
                min = 0, max = max(murders_r$population),
                value = range(murders_r$population), sep = ","),
    radioButtons("y_var", "y-axis variable",
                 choices = c("Number of murders" = "total",
                             "Murders per 100,000" = "rate")),
    checkboxInput("show_lm", "Show regression line", value = FALSE)
  ),
  textOutput("summary_text"),
  layout_columns(
    col_widths = c(7, 5),
    card(plotOutput("murder_plot")),
    card(tableOutput("result"))
  )
)

server_4 <- function(input, output) {

  filtered_data <- reactive({
    murders_r %>%
      filter(region == input$selected_region,
             population >= input$pop_range[1],
             population <= input$pop_range[2])
  })

  output$summary_text <- renderText({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No states selected."))
    paste0(nrow(d), " states selected in the ", input$selected_region, " region; ",
           "the average murder rate is ", round(mean(d$rate), 2), " per 100,000.")
  })

  output$murder_plot <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No states match these conditions. Try a wider population range."))

    # .data[[string]] : use an input value (a string) as a variable name
    p <- ggplot(d, aes(population, .data[[input$y_var]])) +
      geom_point(color = "#B57A23", size = 3) +
      geom_text(aes(label = abb), vjust = -1, size = 3) +
      labs(x = "Population", y = input$y_var) +
      theme_minimal()

    if (input$show_lm) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x)
    }
    p
  })

  output$result <- renderTable({
    filtered_data() %>% select(state, population, total, rate)
  })
}

shinyApp(ui_4, server_4)

# Note
#   · When you switch between 'total' and 'rate', which states change rank the most?
#   · If you narrow the slider until no states are left, what message do you see?
#
# [Exercise 4-1] Sort the table in descending order of the y-axis variable.
#            Hint: arrange(desc(.data[[input$y_var]]))
# [Exercise 4-2] How does the layout change if you set col_widths to c(12, 12)?


# ---- Exercise 4-1 ----------------------------------------------
#   output$result <- renderTable({
#     filtered_data() %>%
#       select(state, population, total, rate) %>%
#       arrange(desc(.data[[input$y_var]]))
#   })

# ---- Exercise 4-2 ----------------------------------------------
# Each element takes all 12 columns → the table is stacked below the plot



# ============================================================
# STEP 5. The 4 kinds of reactives: buttons and observe
# ------------------------------------------------------------
#                              returns a value     only does an action
#   runs when inputs change    reactive()          observe()
#   runs on a button click     eventReactive()     observeEvent()
# ============================================================

ui_5 <- page_sidebar(
  title = "Comparing the 4 Reactives",
  sidebar = sidebar(
    selectInput("selected_region", "Region",
                choices = unique(murders$region)),
    actionButton("run_btn",  "Run analysis", class = "btn-primary"),
    actionButton("save_btn", "Save CSV")
  ),
  h5("① reactive(): changes as soon as you change the region"),
  textOutput("auto_text"),
  h5("② eventReactive(): changes only when you click [Run analysis]"),
  tableOutput("btn_table")
)

server_5 <- function(input, output) {

  # ① reactive: recomputes automatically when inputs change, returns a value
  filtered <- reactive({
    murders %>% filter(region == input$selected_region)
  })

  output$auto_text <- renderText({
    paste("Current selection:", input$selected_region, "/", nrow(filtered()), "states")
  })

  # ② eventReactive: computes only when the button is clicked, returns a value
  result <- eventReactive(input$run_btn, {
    filtered() %>%
      summarise(`States`        = n(),
                `Total murders` = sum(total),
                `Murder rate`   = round(sum(total) / sum(population) * 1e5, 2))
  })

  output$btn_table <- renderTable({
    result()
  })

  # ③ observe: runs automatically when inputs change, no value — only an 'action' (watch the Console)
  observe({
    cat("[observe] Selected region:", input$selected_region, "\n")
  })

  # ④ observeEvent: does an 'action' only when the button is clicked (saves a file)
  observeEvent(input$save_btn, {
    fname <- paste0("result_", input$selected_region, ".csv")
    write.csv(filtered(), fname, row.names = FALSE)
    showNotification(paste(fname, "saved to:", getwd()), type = "message")
  })
}

shinyApp(ui_5, server_5)

# Note
#   · If you change only the region, ① changes but ② stays the same → click the button to update ②
#   · Do you see the [observe] log in the RStudio Console?
#   · After [Save CSV], check for the result_*.csv file in the Files tab.


# ============================================================
# STEP 6. Final app + deployment checklist
# ------------------------------------------------------------
# 1) Put the app.R you received in a new folder and open it → [Run App] at the top right
# 2) Checklist before deploying
#    [ ] Is the file named exactly app.R?
#    [ ] Is there no install.packages() inside app.R?
#    [ ] Are data files inside the app folder and read with a relative path ("data.csv")?
#        (no absolute paths like C:/Users/...)
#    [ ] Are all packages you use loaded with library()?
# 3) In the Console (first time only)
#    install.packages("rsconnect")
#    rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#    rsconnect::deployApp("path_to_app_folder")     # or the [Publish] button
#    → Never screenshot or share your Token/Secret
# ============================================================
