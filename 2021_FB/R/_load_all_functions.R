###############################################################################
#
# Compiling C++ code and sourcing all functions
#
###############################################################################

# compiling C/C++ codes (this should be done before loading R functions)
message("Compiling C/C++ code ...")

cpp_names = grep("^.*\\.cpp$", dir(here("src")), value = TRUE)
for (h in cpp_names) 
    Rcpp::sourceCpp(here("src", h))

message("Sourcing R functions ...")

# enable just-in-time bytecode compiling
compiler::enableJIT(1L)

f_names = setdiff(dir(here("R")), "_load_all_functions.R")

for (f in f_names) 
    source(here("R", f))

# remove obs
rm(cpp_names, h, f_names, f)

message("\nDone!")

### END OF CODE ###