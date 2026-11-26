# Data preparation function
prepare_dig_data <- function(dig) {
    
    # Use the Assignment 4 variables
    required_cols <- c(
        "ID", "TRTMT", "AGE", "SEX", "BMI", "CREAT", "DIABP", "SYSBP",
        "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "HOSPDAYS",
        "DEATH", "DEATHDAY", "KLEVEL"
    )
    
    missing_cols <- setdiff(required_cols, names(dig))
    
    if (length(missing_cols) > 0) {
        stop(
            "DIG dataset is missing required columns: ",
            paste(missing_cols, collapse = ", ")
        )
    }
    
    # Select and prepare variables
    dig <- dig %>%
        select(all_of(required_cols)) %>%
        mutate(
            ID = as.character(ID),
            
            TRTMT = factor(
                TRTMT,
                levels = c(0, 1),
                labels = c("Placebo", "Treatment")
            ),
            
            AGE = as.numeric(AGE),
            
            SEX = factor(
                SEX,
                levels = c(1, 2),
                labels = c("Male", "Female")
            ),
            
            BMI = as.numeric(BMI),
            CREAT = as.numeric(CREAT),
            DIABP = as.numeric(DIABP),
            SYSBP = as.numeric(SYSBP),
            
            # Replace extreme potassium value with NA
            KLEVEL = as.numeric(KLEVEL),
            KLEVEL = ifelse(ID == "940", NA_real_, KLEVEL),
            
            HYPERTEN = factor(
                HYPERTEN,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            
            CVD = factor(
                CVD,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            
            WHF = factor(
                WHF,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            
            DIG = factor(
                DIG,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            
            HOSP = factor(
                HOSP,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            
            HOSPDAYS = as.numeric(HOSPDAYS),
            
            DEATH = factor(
                DEATH,
                levels = c(0, 1),
                labels = c("Alive", "Death")
            ),
            
            DEATHDAY = as.numeric(DEATHDAY),
            Month = round(DEATHDAY / 30)
        )
    
    return(dig)
}
