################################################################################
##
## New denominator for Fall 2020
##
################################################################################

# Set up & read data -----------------------------------------------------------

options(scipen = 999)

# libraries
library("here")
library("magrittr")

# current time
message(
    paste0("\nStart running 04_Update_table.R on ", Sys.time())
)

# current objects
obj_list = ls()

# create directory if non-existent
if (!dir.exists(here("output"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("output"), recursive = T)
    
}

# source functions
source(here("R", "_utils.R"))


# New denominators -------------------------------------------------------------

# new vcount in ithaca
n_ithaca = 17986

# new vcount enrolled
n_enroll = 23376

# Read results -----------------------------------------------------------------

# read data
years = c(2019, 2020)
res_list = lapply(
    years, 
    function (w) readRDS(here("data", paste0("res", w, ".rds")))
) 

# read btw. centr. object calculated in 2019
betw_old = readRDS(here("data", "betw2020.rds"))

# check names & combine measures
measures = vector("list", 2L)
for (i in seq_along(res_list[[1]])) {
    
    x19 = res_list[[1]][[i]]
    x20 = res_list[[2]][[i]]
    
    # check names
    stopifnot(
        identical(names(x19), names(x20))
    )
    
    measures[[i]] = cbind(
        unlist(x19), unlist(x20)
    )
}
names(measures) = c("two_mode", "one_mode")

# object to update
updated = measures

# Updating two-mode results ----------------------------------------------------

# get reference 
ref = measures$two_mode

# empty container
x = y = numeric(NROW(ref))

# measure names
m_names = names(x) = names(y) = rownames(ref)

old_n = ref[which(m_names == "n_students"), 2L]
old_k = ref[which(m_names == "n_courses"), 2L]

for (i in seq_along(x)) {
    
    if (m_names[i] == "n_students") {
        
        x[i] = n_ithaca
        y[i] = n_enroll
        
    } else if (
        m_names[i] %in% c("n_courses", "n_edges", "p1_courses", "b1_courses")
    ) {
        
        x[i] = y[i] = ref[i, 2L] 
    
    } else if (m_names[i] %in% c("p1_students", "b1_students")) {
        
        x[i] = update_denom_node(ref[i, 2L], old_n, n_ithaca)
        y[i] = update_denom_node(ref[i, 2L], old_n, n_enroll)
    
    } else if (m_names[i] == "btw_cntr") {
        
        x[i] = update_centr_betw(betw_old, n_ithaca)
        y[i] = update_centr_betw(betw_old, n_enroll)
        
    } else if (m_names[i] == "density") {
        
        x[i] = update_denom_edge(
            ref[i, 2L], c(old_n, old_k), c(n_ithaca, old_k), TRUE
        )
        
        y[i] = update_denom_edge(
            ref[i, 2L], c(old_n, old_k), c(n_enroll, old_k), TRUE
        )
        
    } else {
        
        stop("undefined measure")
        
    }
    
}

# update
updated$two_mode = cbind(updated$two_mode, x, y)



# Updating one-mode results ----------------------------------------------------

# get reference 
ref = measures$one_mode

# empty container
x = y = numeric(NROW(ref))

# measure names
m_names = names(x) = names(y) = rownames(ref)

for (i in seq_along(x)) {
    
    if (m_names[i] %in% c("m1_edges", "m1_trans", "m1_mdist", "m1_diam")) {
        
        x[i] = y[i] = ref[i, 2L]
        
    } else if (m_names[i] == "m1_density") {
        
        x[i] = update_denom_edge(ref[i, 2L], old_n, n_ithaca, FALSE)
        y[i] = update_denom_edge(ref[i, 2L], old_n, n_enroll, FALSE)
        
    } else if (m_names[i] %in% grep("reach", m_names, value = T)) {
        
        x[i] = update_denom_node(ref[i, 2L], old_n, n_ithaca)
        y[i] = update_denom_node(ref[i, 2L], old_n, n_enroll)
        
    } else {
        
        stop("undefined measure")
        
    }
    
}

# update
updated$one_mode = cbind(updated$one_mode, x, y)

# round and transform into string
updated_str = lapply(
    updated, function(w) apply(w, 2L, format_str, 3L)
)

# add trailing zeros to density (if density == "0")
updated_str$two_mode[4L,] = ifelse(
    updated_str$two_mode[4L,] == "0",
    "0.000", 
    updated_str$two_mode[4L,]
)



## Update table ----------------------------------------------------------------


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
m1 = cbind(two_names, updated_str$two_mode) 
m2 = cbind(one_names, updated_str$one_mode)
m3 = m2[6:8, ]
m2 = m2[1:5, ]

# number of measures for each mode
n_measures = sapply(updated_str, NROW)

# create table
tab = rbind(
    c("Social Network Measures", "Fall 19", "Fall 20", "Fall 20 (ithaca)", "Fall 20 (enrolled)"),
    c("2-Mode (Student-to-Course) Network", "", "", "", ""),
    m1,
    c("Projected 1-Mode (Student-to-Student) Network", "", "", "", ""),
    m2,
    c("Proportion reachable in k steps", "", "", "", ""),
    gsub("Network density", "k = 1", m2[2, ]),
    m3
)
rownames(tab) = colnames(tab) = NULL



# Save & get session info ------------------------------------------------------

rio::export(tab, here("output", "table2_updated.xlsx"), col.names = F)
saveRDS(tab, here("data", "table2_updated.rds"))


# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)


### END OF CODE ###