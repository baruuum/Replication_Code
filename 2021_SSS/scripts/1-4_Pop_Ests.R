
## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")

## ------------------------------------------------------------------
## Save population proportion estimates
## ------------------------------------------------------------------

message('Saving population proportion for core groups ... ')

pop_acq = data.table(
    labs = c(
        'acqunemp', 
        'acqhome', 
        'acqprisn', 
        'acqasian', 
        'acqblack', 
        'acqhisp', 
        'acqwhite', 
        'acqgay', 
        'acqcohab', 
        'acqgoatt', 
        'acqnoatt', 
        'acqlib', 
        'acqcon'),
    prop = c(
        .050,
        .061,
        .010,
        .043,
        .122,
        .148,
        .662,
        .014,
        .011,
        .311,	
        .422,
        .088,
        .116),
    sds = c(
        .010,
        .010,
        .005,
        .010,
        .010,
        .010,
        .010,
        .020,
        .010,
        .020,
        .020,
        .019,
        .027),
    source=c(
        'Bureau of Labor Statistics',
        'PSID, 2005',
        'Bureau of Justice Statistics',
        'American Community Survey, 2006',
        'American Community Survey, 2006',
        'American Community Survey, 2006',
        'American Community Survey, 2006',
        'William Institute (National Survey of Family Growth)',
        'Current Population Survey, 2007',
        'GSS, 2006',
        'GSS, 2006',
        'GSS, 2006',
        'GSS, 2006')
)
fwrite(pop_acq, here("data", "pop_est.csv"))


### End of code ###