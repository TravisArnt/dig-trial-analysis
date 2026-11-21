aboutUI <- function(){
    tagList(
        
        h2("About & Data"),

        # 1. Trial information
        fluidRow(
        box(
            title = tagList(icon("file-text"), "The DIG Trial"),
            width = 12,
            p("The Digitalis Investigation Group (DIG) Trial was a randomized clinical trial investigating the effects of digoxin on mortality and hospitalization in patients with heart failure."),
            
            p("This dashboard explores a prepared dataset containing 6,800 patients."))),
        
        # Dataset and download button
        fluidRow(
            box(
                title = tagList(icon("file-csv"),"Dataset"),
                width = 12,
                reactableOutput("table"),
                div(
                    style = "margin-top: 20px;",
                downloadButton(
                    outputId = "downloadData", 
                    label = 'Download CSV file here')
        )
    )
        ))
}


# About Server
aboutServer <- function(output,data) {
    
    # Table
    output$table <- renderReactable({
        reactable(
            data,
            searchable = TRUE,
            filterable = TRUE,
            sortable = TRUE,
            pagination = TRUE,
            defaultPageSize = 10
        )
    })
    
    #Download dataset
    output$downloadData <- downloadHandler(
        
        filename = function() {
            "DIG_cleaned.csv"
        },
        
        content = function(file) {
            write.csv(
                data,
                file,
                row.names = FALSE
            )
        }
    )
}

