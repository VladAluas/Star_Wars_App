# 1. Setup ----------------------------------------------------------------

# 1.1. Libraries ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(plotly)

# 1.2. Data ---------------------------------------------------------------
data <- starwars %>%
    mutate(films = str_replace_all(films, fixed('c("'), ''),
           films = str_replace_all(films, fixed('" '), ''),
           films = str_replace_all(films, fixed('"'), ''),
           films = str_replace_all(films, fixed(' '), ''),
           films = str_replace_all(films, fixed(')'), '')) %>%
    separate(films, 
             into = c("Film 1", "Film 2", "Film 3", "Film 4", "Film 5", "Film 6", "Film 7"), 
             sep = ",") %>%
    gather("Value", "Film", c("Film 1", "Film 2", "Film 3", "Film 4", "Film 5", "Film 6", "Film 7")) %>%
    select(- Value) %>%
    filter(!is.na(Film))


# 1.2.1 Height Data -------------------------------------------------------
data_height <- data %>%
    filter(!is.na(species))

# 1.2.2 Age Data ----------------------------------------------------------
data_age <- 
    rbind(
    data %>%
    mutate(birth_year = ifelse(name == "Plo Koon", 382, round(birth_year)),
           age = ifelse(Film == "ThePhantomMenace", birth_year - 32,
                 ifelse(Film == "AttackoftheClones", birth_year - 22,
                 ifelse(Film == "RevengeoftheSith", birth_year - 19,
                 ifelse(Film == "ANewHope", birth_year,
                 ifelse(Film == "TheEmpireStrikesBack", birth_year + 3,
                 ifelse(Film == "ReturnoftheJedi", birth_year + 4, birth_year + 34))))))) %>%
    filter(!is.na(age)) %>%
    select(name, Film, age),
    
    data %>%
        mutate(birth_year = ifelse(name == "Plo Koon", 382, round(birth_year)),
               age = ifelse(Film == "ThePhantomMenace", birth_year - 32,
                     ifelse(Film == "AttackoftheClones", birth_year - 22,
                     ifelse(Film == "RevengeoftheSith", birth_year - 19,
                     ifelse(Film == "ANewHope", birth_year,
                     ifelse(Film == "TheEmpireStrikesBack", birth_year + 3,
                     ifelse(Film == "ReturnoftheJedi", birth_year + 4, birth_year + 34))))))) %>%
        filter(!is.na(age)) %>%
        group_by(name) %>%
        summarise(Film = "All",
                  age = max(age))
    )

# 1.2.3. Film Data --------------------------------------------------------
data_app <- data %>%
    filter(!is.na(mass), !is.na(height), mass < 250)

# 2. User Interface -------------------------------------------------------

# 2.1. Sidebar ------------------------------------------------------------
# At the moment the sidebar will only have two parts, Graphics tab and Tables tab

# 2.1.1 Graphics ----------------------------------------------------------
graphs_tab <- menuItem(
    text = "Graphics",
    icon = icon("dashboard"),
    tabName = "graph_tab"
)

# 2.1.2. Tables -----------------------------------------------------------
tables_tab <- menuItem(
    text = "Tables",
    icon = icon("table"),
    tabName = "table_tab"
)

# 2.1.3. Creating the sidebar ---------------------------------------------

sidebar <- sidebarMenu(graphs_tab, tables_tab)

# 2.2. Dashboard Body -----------------------------------------------------

# 2.2.1 Grahics Tab -----------------------------------------------------
# 2.2.1.1. First Row ------------------------------------------------------
# This row will be a simple row with buttons that will allow you to select one film or all of them

dash_first_row <- fluidRow(
    actionButton("all", "All Movies", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("tpm", "The Phantom Menace", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("aotc", "Attack of the Clones", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("rots", "Revenge of the Sith", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("anh", "A New Hope", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("tesb", "The Empire Strickes Back", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("rotj", "Return of the Jedi", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("tfa", "The Force Awakens", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
)

# 2.2.1.2. Second Row -----------------------------------------------------
# The Second row will contain the actual graphs

dash_second_row <- 
    fluidRow(
    tabBox(
    id = "tabset1", 
    width = "100%",
    height = "100%",
    
        # Height Distribution
        tabPanel(
            title = h4("Height Distribution"), 
            p("This graph is intended to show the distribution of the height of different species across the Star Wars universe"),
            plotOutput("height_plot")
            ),
        
        # Age Distribution
        tabPanel(
            title = h4("Age Distribution"), 
            p("This is a graph showing the age of different characters in the Star Wars universe\n"), 
            plotOutput("age_plot")
            ),
        
        # Character Appearances V1
        tabPanel(
            title = h4("Character Appearance v1"), 
            p("In this part we will plot the characters by Height and Mass and group them by Home Planet and Species"),
            p(textOutput("char_names", inline = T)),
            plotOutput("appearance_plot_1", hover = hoverOpts("char_hover"))
        ),
    
        # Character Appearances V2
        tabPanel(
            title = h4("Character Appearance v2"), 
            p("In this part we will plot the characters by Height and Mass and group them by Home Planet and Species"),
            plotlyOutput("appearance_plot_2")
        ),
        # Aestetics
        tags$head(tags$style('p {color: grey;
                                font-size: 20px;
                                }')
    )))

# 2.2.1.3 Pulling the rows together ---------------------------------------
dash_graph_tab <- tabItem(
    tabName = "graph_tab",
   dash_first_row, hr(), dash_second_row
)
# 2.2.2. Tables Tab -------------------------------------------------------

dash_table_tab <- tabItem(
    tabName = "table_tab",
    dataTableOutput("table_total")
)

# 2.2.3. Creating the Dashboard body --------------------------------------

dashboard <- tabItems(dash_graph_tab, dash_table_tab)

# 3. Define UI for application ---------------------
ui <- dashboardPage(
    dashboardHeader(title = "Star Wars Analysis"),
    dashboardSidebar(sidebar),
    dashboardBody(dashboard)
)

# 4. Define server logic ------------------
server <- function(input, output, session) {
# Plot Aspect
aspect <- 0.45

hoverValue <- function(hover, x, y, captions, tolerance = 0.05) {
    if (!is.null(hover)) {
        x0 <- hover$x # x coordinate in user space
        y0 <- hover$y # y coordinate in user space
        xrange <- hover$domain$right - hover$domain$left
        yrange <- hover$domain$top - hover$domain$bottom
        # find the index of the observation closest in scaled 1-norm to the mouse
        dist <- abs(x0 - x) / xrange + abs(y0 - y) / yrange
        i <- which.min(dist)
        # return the corresponding index if close enough, else NULL
        if (dist[i] < tolerance) {
            cat(paste("Name: ", captions[i], sep = ""))
        } else {
            cat("Name: ")
        }
    } else {
        cat("Name: ")
    }
}

# HEIGHT PLOT
# Setting the ACTION BUTTONS 
    
    hgt <- reactiveValues(df = data_height)
    
    observeEvent(input$all,  {hgt$df <- data_height})
    observeEvent(input$tpm,  {hgt$df <- data_height %>% filter(Film == "ThePhantomMenace")})
    observeEvent(input$aotc, {hgt$df <- data_height %>% filter(Film == "AttackoftheClones")})
    observeEvent(input$rots, {hgt$df <- data_height %>% filter(Film == "RevengeoftheSith")})
    observeEvent(input$anh,  {hgt$df <- data_height %>% filter(Film == "ANewHope")})
    observeEvent(input$tesb, {hgt$df <- data_height %>% filter(Film == "TheEmpireStrikesBack")})
    observeEvent(input$rotj, {hgt$df <- data_height %>% filter(Film == "ReturnoftheJedi")})
    observeEvent(input$tfa,  {hgt$df <- data_height %>% filter(Film == "TheForceAwakens")})

# Height Plot
    output$height_plot <- renderPlot({
        hgt$df %>%
            ggplot(mapping = aes(x = str_replace(hgt$df$species, " ", "\n"), y = hgt$df$height)) +
            geom_boxplot(colour = "#557EB6") +
            labs(x = "Species", y = "Height") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1),
                  axis.text.y = element_text(size = 12),
                  axis.title = element_text(size = 18))
        },
        height = function() {
            aspect * session$clientData$output_height_plot_width
        })
    
# AGE PLOT
# Setting the ACTION BUTTONS 
    
    yo <- reactiveValues(df = data_age %>% filter(Film == "All"))
    
    observeEvent(input$all,  {yo$df <- data_age %>% filter(Film == "All")})
    observeEvent(input$tpm,  {yo$df <- data_age %>% filter(Film == "ThePhantomMenace")})
    observeEvent(input$aotc, {yo$df <- data_age %>% filter(Film == "AttackoftheClones")})
    observeEvent(input$rots, {yo$df <- data_age %>% filter(Film == "RevengeoftheSith")})
    observeEvent(input$anh,  {yo$df <- data_age %>% filter(Film == "ANewHope")})
    observeEvent(input$tesb, {yo$df <- data_age %>% filter(Film == "TheEmpireStrikesBack")})
    observeEvent(input$rotj, {yo$df <- data_age %>% filter(Film == "ReturnoftheJedi")})
    observeEvent(input$tfa,  {yo$df <- data_age %>% filter(Film == "TheForceAwakens")})
    
# Age Plot
    output$age_plot <- renderPlot({
        yo$df %>%
            ggplot(mapping = aes(x = reorder(yo$df$name, yo$df$age), y = yo$df$age)) +
            geom_col(fill = "#557EB6") +
            scale_y_continuous(expand = c(0,0), limits = c(0, max(yo$df$age) + 50)) +
            coord_flip() +
            geom_text(mapping = aes(label = yo$df$age), hjust = - 0.5) +
            labs(x = element_blank(), y = "Age") +
            theme_bw() +
            theme(axis.text.x = element_text(size = 12, hjust = 1),
                  axis.text.y = element_text(size = 12),
                  axis.title = element_text(size = 18))
        },
        height = function() {
            aspect * session$clientData$output_height_plot_width
        })

# Appearance PLOT
# Setting the ACTION BUTTONS 
    
    app <- reactiveValues(df = data_app)
    
    observeEvent(input$all,  {app$df <- data_app})
    observeEvent(input$tpm,  {app$df <- data_app %>% filter(Film == "ThePhantomMenace")})
    observeEvent(input$aotc, {app$df <- data_app %>% filter(Film == "AttackoftheClones")})
    observeEvent(input$rots, {app$df <- data_app %>% filter(Film == "RevengeoftheSith")})
    observeEvent(input$anh,  {app$df <- data_app %>% filter(Film == "ANewHope")})
    observeEvent(input$tesb, {app$df <- data_app %>% filter(Film == "TheEmpireStrikesBack")})
    observeEvent(input$rotj, {app$df <- data_app %>% filter(Film == "ReturnoftheJedi")})
    observeEvent(input$tfa,  {app$df <- data_app %>% filter(Film == "TheForceAwakens")})
  
    
# Appearance Plot 1
    output$appearance_plot_1 <- renderPlot({
            app$df %>%
                ggplot(mapping = aes(x = app$df$mass, 
                                     y = app$df$height, 
                                     text = paste(app$df$name, "<br />", app$df$species))) +
                geom_jitter(mapping = aes(colour = app$df$species), alpha = 0.5) +
                scale_x_continuous(expand = c(0,0), limits = c(0, 175)) +
                #geom_text(mapping = aes(label = name, colour = species)) +
                labs(x = "Mass", y = "Height") +
                facet_grid(app$df$gender ~ .) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 12, hjust = 1),
                      axis.text.y = element_text(size = 12),
                      axis.title = element_text(size = 18),
                      legend.title = element_blank())
        },
        height = function() {
            (aspect - 0.03) * session$clientData$output_height_plot_width
        })   
    
    output$char_names <- renderPrint({hoverValue(input$char_hover, app$df$mass, app$df$height, app$df$name)})
      
# Appearance Plot 2
    output$appearance_plot_2 <- renderPlotly({
        ggplotly(
            app$df %>%
                ggplot(mapping = aes(x = app$df$mass, 
                                     y = app$df$height, 
                                     text = paste(app$df$name, "<br />", app$df$species))) +
                geom_jitter(mapping = aes(colour = app$df$species), alpha = 0.5) +
                scale_x_continuous(expand = c(0,0), limits = c(0, 175)) +
                #geom_text(mapping = aes(label = name, colour = species)) +
                labs(x = "Mass", y = "Height") +
                facet_grid(app$df$gender ~ .) +
                theme_bw() +
                theme(legend.position = "none"), 
            tooltip = "text")
        })
    
    output$table_total <- renderDataTable(data)    

}

# Try to make the values show when hovering over them
# Table Output

# 5. Run the application ------------
shinyApp(ui = ui, server = server)











