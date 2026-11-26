overviewUI <- function() {
    tagList(
        h2("Overview"),
        
        fluidRow(
            box(
                width = 3,
                div(
                    style = "text-align: center;",
                    h3(icon("users"), " ",
                       textOutput("total_patients", inline = TRUE)),
                    p("Participants")
                )
            ),
            
            box(
                width = 3,
                div(
                    style = "text-align: center;",
                    h3(icon("medkit"), " ",
                       textOutput("total_treatment", inline = TRUE)),
                    p("Digoxin")
                )
            ),
            
            box(
                width = 3,
                div(
                    style = "text-align: center;",
                    h3(icon("flask"), " ",
                       textOutput("total_placebo", inline = TRUE)),
                    p("Placebo")
                )
            ),
            
            box(
                width = 3,
                div(
                    style = "text-align: center;",
                    h3(icon("heartbeat"), " ",
                       textOutput("mortality_rate", inline = TRUE)),
                    p("Mortality")
                )
            )
        ),
        
        # Filter dropdowns
        fluidRow(
            box(
                width = 12,
                title = "Patient Filters",
                
                fluidRow(
                    column(
                        width = 3,
                        sliderInput(
                            "age_range", # Input id to use in server
                            "Age Range (years)", # Label (Shown in UI)
                            min = 18,
                            max = 100,
                            value = c(18, 100),
                            width = "100%"
                        )
                    ),
                    
                    column(
                        width = 3,
                        selectInput(
                            "sex",
                            "Sex",
                            choices = c("All", "Male", "Female"),
                            width = "100%"
                        )
                    ),
                    
                    column(
                        width = 3,
                        selectInput(
                            "treatment",
                            "Treatment Group",
                            choices = c(
                                "All",
                                "Digoxin",
                                "Placebo"
                            ),
                            width = "100%"
                        )
                    ),
                    
                    column(
                        width = 3,
                        br(),
                        actionButton(
                            "reset_filters",
                            "Reset Filters",
                            icon = icon("refresh")
                        )
                    )
                )
            )
        ),
        
        ## Outcomes by treatment 
        fluidRow(
            box(
                title = "Outcomes by Treatment",
                width = 8,
                plotlyOutput("outcome_plot", height = "350px")
            )
        )
    )
    
}


overviewServer <- function(input,output,session, data) {
    
    # Filtering function
    filtered_dig <- reactive({
        
        # Age filter
        df<- data  %>%
            filter(
                AGE >= input$age_range[1],
                AGE <= input$age_range[2]
            )
        # Sex filter
        if (input$sex!= "All"){
            df <- df  %>%
                filter(SEX == input$sex)
        }
        
        # Clinical subgroup
        if (input$treatment != "All") {
            selected_treatment <- if (input$treatment == "Digoxin") {
                "Treatment"
            } else {
                "Placebo"
            }
            
            df <- df %>%
                filter(TRTMT == selected_treatment)
        }
        df
    })
   
    # Cards
    output$total_patients <- renderText({
        format(nrow(filtered_dig()), big.mark = ",")
    })
    
    output$total_treatment <- renderText({
        format(
            sum(filtered_dig()$TRTMT == "Treatment", na.rm = TRUE),
            big.mark = ","
        )
    })
    
    output$total_placebo <- renderText({
        format(
            sum(filtered_dig()$TRTMT == "Placebo", na.rm = TRUE),
            big.mark = ","
        )
    })
    
    output$mortality_rate <- renderText({
        df <- filtered_dig()
        
        if (nrow(df) == 0) return("N/A")
        
        paste0(
            round(mean(df$DEATH == "Death", na.rm = TRUE) * 100, 1),
            "%"
        )
    })
    
    ## Reset filters button control
    observeEvent(input$reset_filters, {
        updateSliderInput(
            session, "age_range",
            value = c(18, 100)
        )
        
        updateSelectInput(
            session, "sex",
            selected = "All"
        )
        
        updateSelectInput(
            session, "treatment",
            selected = "All"
        )
    })
    
    # Outcome by treatment plot
    output$outcome_plot <- renderPlotly({
        
        df <- filtered_dig()
        
        # Mortality
        mortality <- df %>%
            group_by(TRTMT) %>%
            summarise(
                Count = sum(DEATH == "Death", na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(Outcome = "Mortality")
        
        # Hospitalization
        hospitalization <- df %>%
            group_by(TRTMT) %>%
            summarise(
                Count = sum(HOSP == "Yes", na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(Outcome = "Hospitalization")
        
        # Worsening heart failure
        worsening_hf <- df %>%
            group_by(TRTMT) %>%
            summarise(
                Count = sum(WHF == "Yes", na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(Outcome = "Worsening HF")
        
        plot_data <- rbind(
            mortality,
            hospitalization,
            worsening_hf
        )
        
        p <- ggplot(plot_data,
               aes(x = Outcome, y = Count, fill = TRTMT)) +
            geom_col(position = "dodge") +
            labs(
                x = NULL,
                y = "Number of patients",
                fill = "Treatment"
            ) +
            theme_minimal()
        ggplotly(p)
    })
}
