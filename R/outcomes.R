outcomesUI <- function() {
    tagList(
        h2("Treatment Outcomes"),
        # Filter dropdowns
        fluidRow(
            box(
                width = 12,
                fluidRow(
                    column(
                        width = 5,
                        selectInput(
                            "outcome_",
                            "Outcome",
                            choices = c("Cardiovascular Disease Hospitalization" = "CVD", "Worsening Heart Failure Hospitalization" = "WHF",
                                        "Digoxin Toxicity Hospitalization" = "DIG", "Hospitalization" = "HOSP", "Death" = "DEATH"),
                            width = "100%"
                        )
                    ),
                    
                    column(
                        width = 3,
                        selectInput(
                            "event_num",
                            "Display",
                            choices = c(
                                "Event Rate %" = "rate",
                                "Absolute Values" = "count"
                            ),
                            width = "100%"
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
                plotlyOutput("outcome_treatment_plot", height = "350px")
            )
        )
    )
    
}


outcomesServer <- function(input,output,session, data) {
    
    # Reactive summarises
    outcome_summary <- reactive({
        req(input$outcome_)
        
        var <- input$outcome_
        event_level <- if (var =="DEATH") "Death" else "Yes"
        
        data %>%
            group_by(TRTMT) %>%
            summarise(
                n_total = n(),
                n_events = sum(.data[[var]] == event_level, na.rm = TRUE)
            ) %>%
            mutate(
                rate = n_events / n_total * 100,
                TRTMT = recode(TRTMT, "Treatment" = "Digoxin")
    )})
    
    
    # Outcome by treatment plot
    output$outcome_treatment_plot <- renderPlotly({
        df <- outcome_summary()
        
        if (input$event_num == "rate") {
            df$value <- df$rate
            y_lab <- "Event rate (%)"
        } else{
                df$value <- df$n_events
                y_lab <- "Number of Patients"}

        
        plot_out <- ggplot(df,
                    aes(x = TRTMT, y = value, fill = TRTMT)) +
            geom_col() +
            labs(
                x = NULL,
                y = y_lab
            ) +
            theme_minimal()
        ggplotly(plot_out)
    })
}
