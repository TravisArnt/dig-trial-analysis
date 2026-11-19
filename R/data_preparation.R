prepare_dig_data <- function(dig) {
        # Use the Assignment 4 variables, excluding KLEVEL.
        required_cols <- c(
            "ID", "TRTMT", "AGE", "SEX", "BMI", "CREAT", "DIABP", "SYSBP",
            "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "HOSPDAYS",
            "DEATH", "DEATHDAY"
        )
        
        missing_cols <- setdiff(required_cols, names(dig))
        if (length(missing_cols) > 0) {
            stop("DIG dataset is missing required columns: ",
                 paste(missing_cols, collapse = ", "))
        }
        
        dig <- dplyr::select(dig, dplyr::all_of(required_cols))
        
        dig <- dplyr::mutate(
            dig,
            ID = as.character(ID),
            TRTMT = factor(TRTMT, levels = c(0, 1),
                           labels = c("Placebo", "Treatment")),
            AGE = as.numeric(AGE),
            SEX = factor(SEX, levels = c(1, 2),
                         labels = c("Male", "Female")),
            BMI = as.numeric(BMI),
            CREAT = as.numeric(CREAT),
            DIABP = as.numeric(DIABP),
            SYSBP = as.numeric(SYSBP),
            HYPERTEN = factor(HYPERTEN, levels = c(0, 1),
                              labels = c("No", "Yes")),
            CVD = factor(CVD, levels = c(0, 1),
                         labels = c("No", "Yes")),
            WHF = factor(WHF, levels = c(0, 1),
                         labels = c("No", "Yes")),
            DIG = factor(DIG, levels = c(0, 1),
                         labels = c("No", "Yes")),
            HOSP = factor(HOSP, levels = c(0, 1),
                          labels = c("No", "Yes")),
            HOSPDAYS = as.numeric(HOSPDAYS),
            DEATH = factor(DEATH, levels = c(0, 1),
                           labels = c("Alive", "Death")),
            DEATHDAY = as.numeric(DEATHDAY),
            Month = round(DEATHDAY / 30)
        )
        
        return(dig)
    }

