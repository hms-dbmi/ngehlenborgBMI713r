library(tidyverse)

agelttwo_to_year <- function(neiss_date) {
    (as.numeric(neiss_date) - 200) / 12
}

neiss16 <- read_tsv(
    "data-raw/neiss2016.tsv",
    col_types = cols()
) %>%
    mutate(Year = "2016")
neiss17 <- read_tsv(
    "data-raw/neiss2017.tsv",
    col_types = cols()
) %>%
    mutate(Year = "2017")
products <- read_tsv(
    "data-raw/products.txt",
    col_types = cols()
)
diagnoses <- read_tsv(
    "data-raw/diagnoses.txt",
    col_types = cols()
)
bdypt <- read_tsv(
    "data-raw/bdypt.txt",
    col_types = cols()
)
neiss1617 <- bind_rows(neiss16, neiss17)
neiss1617 <- left_join(neiss1617, products, by = c("Product_1" = "Code")) %>%
    left_join(diagnoses, by = c("Diagnosis" = "Code")) %>%
    left_join(bdypt, by = c("Body_Part" = "Code"))


neiss1617 <- neiss1617 %>%
    mutate(
        Age = ifelse(Age > 150, agelttwo_to_year(Age), Age),
        age_group = case_when(
            Age < 2  ~ "infant",
            Age < 11 ~ "children",
            Age < 20 ~ "adolescent",
            Age < 30 ~ "20-29",
            Age < 40 ~ "30-39",
            Age < 50 ~ "40-49",
            Age < 60 ~ "50-59",
            Age < 70 ~ "60-69",
            Age < 80 ~ "70-79",
            Age < 90 ~ "80-89",
            Age < 100 ~ "90-99",
            TRUE ~ "100+"
        )
    )


usethis::use_data(neiss1617)
