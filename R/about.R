aboutUI <- function(){
    tagList(
        h2("About & Data"),
        
        reactableOutput("table")
    )
}

# About Server
aboutServer <- function(output,data) {
    
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
}

