

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
                    ),
                    
                    column(
                        width = 3,
                        selectInput(
                            "sub_groups",
                            "Subgroup",
                            choices = c(
                                "Sex" = "SEX",
                                "Hypertension" = "HYPERTEN",
                                "BMI" = "BMI_categories"
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
                width = 6,
                plotlyOutput("outcome_treatment_plot", height = "350px")
            ),
            box(
                title = "Outcomes Across Subgroups",
                width = 6,
                plotlyOutput("outcome_subgroup_plot", height = "350px")
            ),
            box(
                title = "Treatment Comparison",
                width = 12,
                reactableOutput("comparison_table")
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
    
    # Outcomes by Subgroups
    subgroup_summary <- reactive({
        req(input$outcome_, input$sub_groups)
        
        var <- input$outcome_
        sub_var <- input$sub_groups
        event_level <- if (var =="DEATH") "Death" else "Yes"
        
        data %>%
            filter(!is.na(.data[[sub_var]])) %>%
            group_by(Subgroup = .data[[sub_var]], TRTMT) %>%
            summarise(
                n_total = n(),
                n_events = sum(.data[[var]] == event_level, na.rm = TRUE)
            ) %>%
            mutate(
                rate = n_events / n_total * 100,
                TRTMT = recode(TRTMT, "Treatment" = "Digoxin")
            )})
    
    # Outcomes by Subgroups Plot
    output$outcome_subgroup_plot <- renderPlotly({
        df <- subgroup_summary()
        
        if (input$event_num == "rate") {
            df$value <- df$rate
            y_lab <- "Event rate (%)"
        } else{
            df$value <- df$n_events
            y_lab <- "Number of Patients"} 
    
    
    plot_sub <- ggplot(df,
                aes(x = Subgroup, y = value, fill = TRTMT)) + 
            geom_col(position = "dodge") +
            labs(
                x = NULL,
                y = y_lab,
                fill = "Treatment"
            ) +
            theme_minimal()
        ggplotly(plot_sub)
    })
    
    
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
    
    # Treatment Comparison table
    outcome_labels = c("Cardiovascular Disease Hospitalization" = "CVD", "Worsening Heart Failure Hospitalization" = "WHF",
                                 "Digoxin Toxicity Hospitalization" = "DIG", "Hospitalization" = "HOSP", "Death" = "DEATH")
    
    
    output$comparison_table <- renderReactable({
        df <- outcome_summary()
        dig <- df %>% filter(TRTMT == "Digoxin")
        placebo <- df %>% filter(TRTMT == "Placebo")
        
        table_df <- data.frame(
            Outcome    = names(outcome_labels)[outcome_labels == input$outcome_],
            Digoxin    = paste0(dig$n_events, " / ", dig$n_total, " (", round(dig$rate,1), "%)"),
            Placebo    = paste0(placebo$n_events, " / ", placebo$n_total, " (", round(placebo$rate,1), "%)"),
            Difference = round(dig$rate - placebo$rate,1)
    )
        reactable(table_df)
        })
    
    
    
}
