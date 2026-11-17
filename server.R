function(input, output, session) {
    output$age_hist <- renderPlot({
        ggplot(dig, aes(x = AGE)) +
            geom_histogram(bins = 30) +
            labs(title = "Age distribution", x = "Age (years)", y = "Count")
    })
}