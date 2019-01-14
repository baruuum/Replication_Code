###############################################################################
##                                                                           ##
##  Numbers Cited in the Text                                                ##
##                                                                           ##
###############################################################################

## Basic Setup ----------------------------------------------------------------

# check base directory
if (!exists('wd.base'))
   stop('Set path to base directory!')

# set paths
if (!exists('code.path'))
   code.path <- paste0(wd.base, 'code/')
if (!exists('data.path'))
   data.path <- paste0(wd.base, 'data/')
if (!exists('raw.data.path')) 
   raw.data.path <- paste0(wd.base, 'raw_data/')
if (!exists('graphics.path')) 
   graphics.path <- paste0(wd.base, 'plots/')
if (!exists('stan.path'))
   stan.path <- paste0(wd.base, 'stan/')


## Generate Numbers Cited in Text ---------------------------------------------

### Issue Partisanship

comb.dat <- readRDS(paste0(data.path, 'comb.dat.rds'))

## %lib on guar.jobs.n.income

# aggregate
comb.dat[ 
   i.vars.label=='guar.jobs.n.income' & year %in% c(1972, 2016), props
]

# Democrats
comb.dat[
   i.vars.label=='guar.jobs.n.income' & year %in% c(1972, 2016), props.d
]

# Republicans
comb.dat[
   i.vars.label=='guar.jobs.n.income' & year %in% c(1972, 2016), props.r
]


### Descriptive Results

## rapidly changing issues

comb.dat[
   i.vars.label=='women.role' & year %in% c(1972, 2008), props
]

comb.dat[
   i.vars.label=='premarital.sex' & year %in% c(1972, 2016), props
]

comb.dat[
   !is.na(props) & i.vars.label=='gay.adoption'
][ 
   year %in% c(min(year), max(year)), .(year, props)
][ 
   ,diff := max(props) - min(props)
][]

comb.dat[
   !is.na(props) & i.vars.label=='gay.military'
][ 
   year %in% c(min(year), max(year)), .(year, props)
][ 
   ,diff := max(props) - min(props)
][]

comb.dat[
   !is.na(props) & i.vars.label=='gay.discrimination'
][ 
   year %in% c(min(year), max(year)), .(year, props)
][ 
   ,diff := max(props) - min(props)
][]

comb.dat[
   i.vars.label == 'women.role' & year %in% c(1972, 2008), props
] %>%
   c(.[2]-.[1])

comb.dat[
   i.vars.label == 'gay.marriage' & year %in% c(1988, 2016), props
]%>%
   c(.[2]-.[1])

comb.dat[
   i.vars.label=='legal.marijuana' & year %in% c(1973, 2016), props
]%>%
   c(.[2]-.[1])



# Regressing issues on time (see. footnote 9 of paper)

tmp <- comb.dat[
   !is.na(props), list(min(year), max(year)), by = i.vars.label
] %>%
   merge(comb.dat, ., by='i.vars.label')

tmp <- tmp[
   , d:= props[year==V2] - props[year==V1], by=i.vars.label
][
   , list(i.vars.label,i.vars.class,d)
] %>%
   unique

tmp[, sum(d<0)/.N, by=i.vars.class]
m.vars <- unique(comb.dat[, i.vars.label])
tmp <- lapply(m.vars, function (w) {
         z <- summary(
             lm(comb.dat[i.vars.label == w, props] ~ 
                   comb.dat[i.vars.label == w, year])
             )
         s <- sign(z$coefficients[2,1])
         p <- z$coefficients[2,4] <= .1
         return(c(s, p))
      }
   ) %>% 
   do.call(rbind,.) %>%
   data.table %>%
   `[`(,i.vars.label:=m.vars) %>%
   setnames(c('d','p','i.vars.label'))

tmp <- merge(tmp, comb.dat[,.(i.vars.label, i.vars.class)], by='i.vars.label')

tmp[p==1, sum(d==1)/.N, by=i.vars.class]


### Multilevel Beta Regression Results

mu.gamma.res <- readRDS(paste0(data.path, 'mu.gamma.res.rds'))

# partisan difference (Dem - Rep) in 1972, for each domain
mu.gamma.res[
   , mean(inv.logit(`mu_gamma[1]` + `mu_gamma[3]`)  - 
             inv.logit(`mu_gamma[1]`)),
   by = domain
][
   , domain := dplyr::recode(domain,
                            '1' = 'Economic',
                            '2' = 'Civil Rights',
                            '3' = 'Moral',
                            '4' = 'Foreign Policy')
] %>% 
   setnames(c('Domain', 'Rep. - Dem.')) %>%
   print

# post. prob that linear time trend is negative for Republicans
mu.gamma.res[
   , mean(ifelse(`mu_gamma[4]` < 0, 1,0)), by = domain
][
   , domain := dplyr::recode(domain,
                            '1' = 'Economic',
                            '2' = 'Civil Rights',
                            '3' = 'Moral',
                            '4' = 'Foreign Policy')
] %>% 
   setnames(c('Domain', 'Post. Prob.')) %>%
   print

# post. prob that linear time trend is positive for Republicans
mu.gamma.res[
   , mean(ifelse(`mu_gamma[4]` > 0, 1,0)), by = domain
][
   , domain := dplyr::recode(domain,
                            '1' = 'Economic',
                            '2' = 'Civil Rights',
                            '3' = 'Moral',
                            '4' = 'Foreign Policy')
] %>% 
   setnames(c('Domain', 'Post. Prob.')) %>%
   print


# post. prob that linear time trend is positive for Democrats
mu.gamma.res[
   , mean(ifelse(`mu_gamma[4]` + `mu_gamma[6]` > 0, 1,0)), by = domain
][
   , domain := dplyr::recode(domain,
                            '1' = 'Economic',
                            '2' = 'Civil Rights',
                            '3' = 'Moral',
                            '4' = 'Foreign Policy')
] %>% 
   setnames(c('Domain', 'Post. Prob.')) %>%
   print


# post. prob that linear time trend is positive for Independents
mu.gamma.res[
   , mean(ifelse(`mu_gamma[4]` + `mu_gamma[5]` > 0, 1,0)), by = domain
][
   , domain := dplyr::recode(domain,
                            '1' = 'Economic',
                            '2' = 'Civil Rights',
                            '3' = 'Moral',
                            '4' = 'Foreign Policy')
] %>% 
   setnames(c('Domain', 'Post. Prob')) %>%
   print


### Using Different Coding Schemes for Variables with No Natural Midpoint

# ## GSS
# dat <- readRDS(paste0(data.path, 'gss.var.select.rds'))
# dat[, p3:= ifelse(partyid %in% 1:3, 1, 
#                   ifelse(partyid %in% 5:7, 2, NA))]
# 
# # abortion
# nn <- grep('^ab', names(dat), value=T)
# par(mfrow=c(2,4))
# for (n in nn) {
#    dat[, tmp := as.numeric(get(n)==1)]
#    dat[get(n) %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=n, type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
#    
# }
# 
# # suicide
# nn <- grep('^suicide', names(dat), value=T)
# par(mfrow=c(2,2))
# for (n in nn) {
#    dat[, tmp := as.numeric(get(n)==1)]
#    dat[get(n) %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=n, type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
#    
# }
# 
# 
# # homosex
# vv <- 1:4
# ll <- c('Always wrong', 
#         'Almost always wrong', 
#         'Wrong only sometimes', 
#         'Not wrong at all')
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(homosex == v)]
#    dat[ homosex %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(homosex > v)]
#    dat[ homosex %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# # premarsx
# vv <- 1:4
# ll <- c('Always wrong', 
#         'Almost always wrong', 
#         'Wrong only sometimes', 
#         'Not wrong at all')
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(premarsx == v)]
#    dat[ premarsx %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(premarsx > v)]
#    dat[ premarsx %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# 
# 
# # teensex
# vv <- 1:4
# ll <- c('Always wrong', 
#         'Almost always wrong', 
#         'Wrong only sometimes', 
#         'Not wrong at all')
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(teensex == v)]
#    dat[ teensex %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(teensex > v)]
#    dat[ teensex %in% c(0,5,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# # xmarsex
# vv <- 1:4
# ll <- c('Always wrong', 
#         'Almost always wrong', 
#         'Wrong only sometimes', 
#         'Not wrong at all')
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(xmarsex == v)]
#    dat[ xmarsex %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(xmarsex > v)]
#    dat[ xmarsex %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# 
# 
# # pronlaw
# vv <- 1:3
# ll <- c('whatever age',
#         'under 18',
#         'no laws')
# for (v in vv) {
#    dat[, tmp := as.numeric(pornlaw == v)]
#    dat[ pornlaw %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# 
# }
# 
# par(mfrow=c(2,2))
# for (v in vv) {
#    dat[, tmp := as.numeric(pornlaw > v)]
#    dat[ pornlaw %in% c(0,8,9), tmp := NA]
#    dd <- dat[, sum(wtssnr*tmp, na.rm=T)/sum(wtssnr[!is.na(tmp)]), by =list(year,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(year,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(year,V1)], pch=18, col='red', type='b')
# }
# 
# 
# ### NES
# dat <- readRDS(paste0(data.path, 'nes.merged.72.16.rds'))
# dat[, p3:=ifelse(VCF0301 %in% 1:3,1, ifelse(VCF0301 %in% 5:7,2,NA))]
# 
# # school prayer
# vv <- 1:4
# ll <- c('Not allowed', 
#         'if want', 
#         'not particular faith', 
#         'Christian prayer')
# par(mfrow=c(2,2))
# for (v in vv) {
#    dat[, tmp := as.numeric(VCF9043 == v)]
#    dat[ VCF9043 %in% c(0,9), tmp := NA]
#    dd <- dat[, sum(VCF0009x*tmp, na.rm=T)/sum(VCF0009x[!is.na(tmp)]), by =list(VCF0004,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(VCF0004,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(VCF0004,V1)], pch=18, col='red', type='b')
# }
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(VCF9043 >v)]
#    dat[ VCF9043 %in% c(0,9), tmp := NA]
#    dd <- dat[, sum(VCF0009x*tmp, na.rm=T)/sum(VCF0009x[!is.na(tmp)]), by =list(VCF0004,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(VCF0004,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(VCF0004,V1)], pch=18, col='red', type='b')
# }
# 
# 
# # abortion
# vv <- 1:4
# ll <- c('Never', 
#         'RapeIncestDanger', 
#         'Need established', 
#         'Always')
# 
# for (v in vv) {
#    dat[, tmp := as.numeric(VCF0838 == v)]
#    dat[ VCF0838 %in% c(0,9), tmp := NA]
#    dd <- dat[, sum(VCF0009x*tmp, na.rm=T)/sum(VCF0009x[!is.na(tmp)]), by =list(VCF0004,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(VCF0004,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(VCF0004,V1)], pch=18, col='red', type='b')
# }
# 
# par(mfrow=c(2,2))
# vv <- 1:4
# for (v in vv) {
#    dat[, tmp := as.numeric(VCF0838 > v)]
#    dat[ VCF0838 %in% c(0,9), tmp := NA]
#    dd <- dat[, sum(VCF0009x*tmp, na.rm=T)/sum(VCF0009x[!is.na(tmp)]), by =list(VCF0004,p3)][!is.na(V1) & !is.na(p3)]
#    plot(dd[p3==1 ,.(VCF0004,V1)] , main=ll[v], type='b', pch=18,
#         ylim=c(0,1), col='blue')
#    points(dd[p3==2, .(VCF0004,V1)], pch=18, col='red', type='b')
# }
