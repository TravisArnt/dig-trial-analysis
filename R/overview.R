overviewUI <- function() {
    tagList(
        h2("Overview"),
        
        fluidRow(
            box(
                width = 3,
                div(
                    class="kpi-card",
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
        )
    )
}


overviewServer <- function(output, data) {
   
    output$total_patients <- renderText({
        format(nrow(data), big.mark = ",")
    })
    
    output$total_treatment <- renderText({
        format(
            sum(data$TRTMT == "Treatment", na.rm = TRUE),
            big.mark = ","
        )
    })
    
    output$total_placebo <- renderText({
        format(
            sum(data$TRTMT == "Placebo", na.rm = TRUE),
            big.mark = ","
        )
    })
    
    output$mortality_rate <- renderText({
        paste0(
            round(mean(data$DEATH == "Death", na.rm = TRUE) * 100, 1),
            "%"
        )
    })
}
