aboutUI <- function(){
    tagList(
        h2("About & Data"),
        
        reactable::reactableOutput("table")
    )
}

# About Server
aboutServer <- function(output,data) {
    
    output$table <- reactable::renderReactable({
        reactable::reactable(
            data,
            searchable = TRUE,
            filterable = TRUE,
            sortable = TRUE,
            pagination = TRUE,
            defaultPageSize = 10
        )
    })
}

