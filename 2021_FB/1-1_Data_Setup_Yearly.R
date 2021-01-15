###############################################################################
##
## Generating Yearly Data for Analysis
##
## Creator     : Barum Park
## Last Update : 02/23/2020
##
###############################################################################


message("\n\n\n***** Setting up yearly data *****")

# Basic Set Up -----------------------------------------------------------------

message("\nChecking setup and paths...")

# check objects
if (!exists("p_vec")) 
    stop("p_vec(period-vector) not found")

if (!exists("start_year")) 
    stop("start_year(starting year) not found")

if (!exists("w_stand_year")) 
    stop("w_stand_year (weight year) not found")

if (!exists("samp_spec")) 
    stop("dat_spec (data specification) not found")

if (!exists("nec_regex"))
    stop("nec_regex not found")

# Loading Data -----------------------------------------------------------------

# if FullSamp is not the last entry of samp_spec, reorder
if (samp_spec[length(samp_spec)] != "FullSamp") {
    
   message("Reordering samp_spec so that FullSamp comes last ...")
   samp_spec = c(setdiff(samp_spec, "FullSamp"), "FullSamp")
   
}


# Read data and save in csv format 

for (s_spec in samp_spec) {

    message(paste0("\n\nWorking on ", s_spec, " data ...\n"))
    message("Reading in raw data ...")

    # get rawdata
    if (s_spec == "FullSamp") {
    
        d_file = "edges_67to15ASEC_dropimp_primeage.dta"
    
    } else if (s_spec == "Male") {
    
        d_file = "edges_67to15ASEC_dropimp_primeage_m.dta"
    
    } else if (s_spec == "Female") {
    
        d_file = "edges_67to15ASEC_dropimp_primeage_f.dta"
    
    } else {
    
        stop("data specification neither FullSamp nor Female nor Male")
    
    }

    dat = readstata13::read.dta13(here("rawdata", d_file)) %>%
        data.table
    dat[, `:=`(int_year = year_begin + 1)]
    dat = dat[, .(occ1, occ2, int_year)]
    
    # dropping pre-1989 years
    message("Dropping pre-1989 years ...")
    dat = dat[int_year >= start_year]

    ### Collapse Instructors & Teacher"s Aides ###
    # note: detailed instructor cateogries as well as the teacher's aides 
    #       category are absent in the 2000 Census occupational classifiation
    #       scheme, but are, for whatever reason, in the CPS.

    message("Merging 'subject instructors' into a single occ category ...")
    
    # get instructor labels
    ins_1 = grep("instruct", dat[, unique(occ1)], value = T, ignore.case = T)
    ins_2 = grep("instruct", dat[, unique(occ2)], value = T, ignore.case = T)
    instructors = c(ins_1, ins_2) %>% unique
    
    # do not merge athlete instructor
    instructors = instructors[
        instructors %in% c("Athletes, sports instructors, and officials",
                           "Subject instructors (HS/college)") == F
    ]
    
    # recode
    dat[occ1 %in% instructors, occ1 := "Subject instructors (HS/college)"]
    dat[occ2 %in% instructors, occ2 := "Subject instructors (HS/college)"]
    
    ### Collapse Teacher"s Aids into Teachers n.e.c ###

    message("Collapsing Teacher's aids into Teachers, n.e.c category ...")
    
    dat[occ1 == "Teacher's aides", occ1 := "Teachers , n.e.c."]
    dat[occ2 == "Teacher's aides", occ2 := "Teachers , n.e.c."]
    
    
    message("Saving recoded data ...")
    
    # save non-aggregated data (needed for bootstrapping later)
    if (s_spec == "FullSamp")
        fwrite(dat[, .(occ1, occ2, int_year)], 
               here("data", paste0("dat_", s_spec, "_FullOccs.csv")))

    # save aggregated data
    fwrite(
        dat[, .N, by = list(occ1, occ2, int_year)], 
        here("data", paste0("dat_", s_spec, "_agg_FullOccs.csv"))
    )
    
    message("Saving recoded data (NEC excluded) ...")
    
    nec_str = paste0(nec_regex, collapse = "|")
    nonec_dat = dat[
        !grepl(nec_str, occ1, ignore.case = TRUE) & 
        !grepl(nec_str, occ2, ignore.case = TRUE)
    ] 
    
    # save non-aggregated data (for bootrstrapping)
    if (s_spec == "FullSamp")
        fwrite(nonec_dat[, .(occ1, occ2, int_year)], 
               here("data", paste0("dat_", s_spec, "_NoNEC.csv")))
    
    fwrite(nonec_dat[, .N, by = list(occ1, occ2, int_year)],
           here("data", paste0("dat_", s_spec, "_agg_NoNEC.csv"))
    )
    
    # Generate weight variables
    
    message("\nGenerating normalizing weights ...")
    message(paste0("Baseline year : ", w_stand_year))
    
    w_year = dat[, .N, int_year]
    n_stand = w_year[int_year == w_stand_year, N]
    w_year[, obs_weights := n_stand/N]

    # save weights
    message("Saving weights for 'FullOccs' into data folder ...")
    fwrite(
        w_year[, -"N"], 
        here("data", paste0("obs_weights_", s_spec, "_FullOccs.csv"))
    )

    # weights with NECs excluded
    w_year = nonec_dat[, .N, int_year]
    n_stand = w_year[int_year == w_stand_year, N]
    w_year[, obs_weights := n_stand/N]
    
    # save weights
    message("Saving weights for 'NoNEC' into data folder ...")
    fwrite(
        w_year[, -"N"], 
        here("data", paste0("obs_weights_", s_spec, "_NoNEC.csv"))
    )
    
}

# Preparing OCC2000 data -------------------------------------------------------

message("\nPreparing OCC2000 data for analysis (NoNEC only) ...")

occ2000_dat = fread(here("rawdata", "edges_post03ASEC_OCC2010.csv"))
occ2000_dat[
    , int_year := year_begin + 1L
][
    , year_begin := NULL
]


# drop NEC occupations
message('Dropping NEC occupations  ... ')

occ2000_dat = occ2000_dat[!(occ1 %in% nec_regex | occ2 %in% nec_regex)]


# get instructor labels
ins_1 = grep("instruct", occ2000_dat[, unique(occ1)], value = T, ignore.case = T)
ins_2 = grep("instruct", occ2000_dat[, unique(occ2)], value = T, ignore.case = T)
instructors = c(ins_1, ins_2) %>% unique

# check that there is only one group of instructors in OCC2000
if (length(instructors) > 1L)
    stop("multiple instructor occupations in OCC2000")

# check that there are no Teacher's aids
if (occ2000_dat[, unique(c(occ1, occ2))] %>% `==`("Teacher's aides") %>% sum != 0)
    stop("occupational category Teacher's aids found in OCC2000")

# creating and adding weights centered at 2003
message("Aggregating and Calculating weights centered at year 2003 ...")



w_year_2000 = occ2000_dat[, .N, int_year]
n_stand = w_year_2000[int_year == min(int_year), N]
w_year_2000[ , obs_weights := n_stand/N]

message("Saving aggregated data and weights ...")

fwrite(
    occ2000_dat[, .N, by = list(occ1, occ2, int_year)],
    here("data", "dat_FullSamp_agg_NoNEC_OCC2000.csv")
)

fwrite(
    w_year_2000[, -"N"],
    here("data", "obs_weights_FullSamp_NoNEC_OCC2000.csv")
)

# Check missing patterns -------------------------------------------------------

message("\nChecking missing pattern (for NoNEC, FullSamp data) ...")

# load NoNEC data
nonec_dat = fread(here("data", "dat_FullSamp_agg_NoNEC.csv"))

# get missing pattern
m_pat = get_missing_pattern(
    dat = nonec_dat, 
    w_size = 5,
    include_isolates = FALSE)

message("Saving data with occs appearing in all 5-year moving windows ...")

# get occupation names
occ_5 = m_pat[m_count_win_5 == 0]$occ_label

# subset data
nonec_dat_5 = nonec_dat[(occ1 %in% occ_5) & (occ2 %in% occ_5)]

# check
if (length(occ_5) != length(nonec_dat_5[, unique(c(occ1, occ2))]))
    stop("dimension mismatch after subsetting")

# save data
fwrite(nonec_dat_5, here("data", "dat_FullSamp_agg_NoNEC_complete5.csv"))



## Create dictionary for occupations (NoNEC) -----------------------------------

message("\nCreating dictionary for occupations and OCC1990 labels (NoNEC) ...")

# get dictionary file
dict = readstata13::read.dta13(here("rawdata", "occ_dictionary.dta")) %>%
    data.table
dict = dict[
    , occ_label := as.character(occ_label)
] %>%
    merge(
        nonec_dat[
            , .(occ_label = unique(c(occ1, occ2)))
        ][
            , occ_id := .I
        ],
        by = "occ_label",
        all.y = TRUE
    ) %>%
    setnames("occ_numeric", "occ_1990")

# check results
if (max(dict$occ_id) != nrow(dict))
    stop("some occ_ids are missing")

if (nrow(dict) != length(nonec_dat[, unique(c(occ1, occ2))]))
    stop("some occupations in the dataset are not in the dictionary")

# save labels
message("Saving dictionary ...")
fwrite(dict, here("data", "labmap_NoNEC.csv"))

# remove unused objects
rm(dat, 
   dict,
   nonec_dat, 
   nonec_dat_5, 
   occ_5,
   m_pat,
   ins_1,
   ins_2, 
   instructors, 
   w_year)

message("\nDone!")

### END OF CODE ###