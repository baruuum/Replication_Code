
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication Code for “Paper Title”

-   Creator: Barum Park

1.  By running the `_MAIN.R`, all results in the paper will be
    replicated.

2.  The raw data have to be placed into a sub-directory of the project’s
    root under the name `/raw_data`. The directory structure when
    running the application should be as follows:

             |
             ├── R
             │   └── test_code
             ├── raw_data
             │   ├── fall2019
             │   │   ├── f2019_uni_field6_studentnbr.xls
             │   │   └── university.net
             │   └── fall2020
             │       ├── f2020_f2f_ith_field6_studentnbr.xls
             │       └── p2_f2f_ith.net
             └── src

    where `f2019_uni_field6_studentnbr.xls` and
    `f2020_f2f_ith_field6_studentnbr.xls` are `Excel` files with two
    columns named `studentnbr`, the student id, and `stgr_field6`, the
    field of study. `university.net` and `p2_f2f_ith.net` should contain
    the student-to-class co-enrollment network data in `Pajek` format.
    Unfortunately, we are not able to share the raw data at this point.

3.  The `_MAIN.R` script will source other scripts, contained in the `R`
    directory. The files `R/_utils.R` and `_plot_utils.R` contain
    user-defined functions that were used throughout the project and the
    `./R/test_code` sub-directory contains unit-tests for these
    functions. When sourcing `_MAIN.R` all unit tests will be run
    automatically.

4.  There are also two compiled `C/C++` functions contained in the `src`
    directory to speed-up the calculations. Documentations regarding
    these functions can be found within the respective `.cpp` files.

5.  The log-file, including the session information, for the last
    replication run can be found in the `_MAIN.log` file.
