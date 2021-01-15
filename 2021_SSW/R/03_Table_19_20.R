################################################################################
##
## Comparison of network statistics between 2019 and 2020 fall
##
################################################################################

# Set up & read data -----------------------------------------------------------

options(scipen = 999)

# libraries
library("here")
library("magrittr")

# current time
message(
    paste0("\nStart running 03_Table_19_20.R on ", Sys.time())
)

# current objects
obj_list = ls()

# create directory if non-existent
if (!dir.exists(here("output"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("output"), recursive = T)
    
}


# Read results -----------------------------------------------------------------

# read data
years = c(2019, 2020)
res_list = lapply(
    years, 
    function (w) readRDS(here("data", paste0("res", w, ".rds")))
) 



# Format measures --------------------------------------------------------------

# check names & combine measures
measures = vector("list", 2L)
for (i in seq_along(res_list[[1]])) {
    
    x19 = res_list[[1]][[i]]
    x20 = res_list[[2]][[i]]
    
    # check names
    stopifnot(
        identical(names(x19), names(x20))
    )
    
    # combine
    measures[[i]] = cbind(
        unlist(x19), unlist(x20)
    ) %>%
        format_str(3)
    
}

# check formatting of density
measures[[1]][4, ] = ifelse(
    measures[[1]][4, ] == "0", 
    "0.000", 
    measures[[1]][4, ]
)



# Create table -----------------------------------------------------------------

# names for table
two_names = c(
    "Number of students (n)",
    "Number of courses (m)",
    "Number of edges (l)",
    "Network density",
    "Proportion of students in largest component",
    "Proportion of courses in largest component",
    "Proportion of students in largest bi-component",
    "Proportion of courses in largest bi-component",
    "Betweenness centralization"
) %>%
    gsub("^", "  ", .)

one_names = c(
    "Network Number of unique edges (l)",
    "Network density",
    "Clustering coefficient (transitivity/closure)",
    "Average geodesic distance",
    "Network diameter (largest observed distance)",
    "k = 2", 
    "k = 3",
    "k = 4"    
) %>% 
    gsub("^", "  ", .)

# create table
m1 = cbind(two_names, measures[[1]]) 
m2 = cbind(one_names, measures[[2]])
m3 = m2[6:8, ]
m2 = m2[1:5, ]

# number of measures for each mode
n_measures = sapply(measures, NROW)

# create table
tab = rbind(
    c("Social Network Measures", "Fall 19", "Fall 20"),
    c("2-Mode (Student-to-Course) Network", "", ""),
    m1,
    c("Projected 1-Mode (Student-to-Student) Network", "", ""),
    m2,
    c("Proportion reachable in k steps", "", ""),
    gsub("Network density", "k = 1", m2[2, ]),
    m3
)
rownames(tab) = colnames(tab) = NULL


# Save table -------------------------------------------------------------------

rio::export(tab, here("output", "table2.xlsx"), col.names = F)
saveRDS(tab, here("data", "table2.rds"))


# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)


### END OF CODE ###