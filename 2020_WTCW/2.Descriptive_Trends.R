#############################################################################
###                                                                       ###
### Descriptive Trends                                                    ###
###                                                                       ###
### Data File Used: comb.dat.rds                                          ###
###                                                                       ###
#############################################################################


## Basic Setup ----------------------------------------------------------------

# check base directory
if (!exists('wd.base'))
   stop('Need wd.base!')

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

# load functions
if (!exists('drop.empty.facets')) 
   source(paste0(code.path, '0.Functions.R'))


message('\n\nPlotting Descriptive Trends ----------')

## Load Data and Set Graph Options --------------------------------------------

# load data
comb.dat <- readRDS(paste0(data.path, 'comb.dat.rds'))

# reshape into long format
plot.dat <- melt(
   comb.dat[, i.vars.no := NULL],
   id.vars = c('i.vars.label',
               'year',
               'i.vars.class',
               'data.source'),
   variable.name = 'subgroup',
   value.name = 'prop.lib',
   variable.factor = F
)

# drop missings (otherwise, line plots will be disconnected) and 
# independents (for clarity)
plot.dat <- plot.dat[!is.na(prop.lib) & subgroup!='props.i'] 

# create indicator of partisan subgroup
plot.dat[, p.group := ifelse(subgroup == 'props', F, T)]

# set ggplot options (common to all graphs)
gg.opts <- list(
   theme_bw(),
   theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text.y = element_blank(),
      strip.text.x = element_text(size = 12,
                                  hjust = .1,
                                  face = 'bold'),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = 'black'),
      legend.position = 'none',
      plot.margin = unit(c(0, .1, .05, .1),
                         units = 'cm')
   ),
   scale_y_continuous(limits = c(0, 1),
                      breaks = c(.2, .5, .8)),
   scale_x_continuous(
      limits = c(1972, 2016),
      breaks = c(1972, 1994, 2016),
      labels = c("'72", "'94", "'16")
   ),
   scale_shape_manual(values = c(19, 68, 82)),
   scale_size_manual(values = c(2.5, 4, 4)),
   scale_color_manual(values = c('black', 'black', 'grey50'))
)

# set col-widths & height scale for plots
col.width <- 1.85
h.scale <- 2.1

# empty list to save plotting order
p.order <- list()


## Economic Variables ---------------------------------------------------------

message('Plotting Economic Issues ...')

# extract economic vars
econ.dat <- plot.dat[i.vars.class == 1]

# list of econ issues
issues <- econ.dat$i.vars.label %>% unique
# number of issues
n.issues <- length(issues)

# number of cols & rows to plot
n.col <- 6L
n.full.row <- floor(n.issues/n.col)
last.row <- n.issues %% n.col
n.row <- n.full.row + as.numeric(last.row > 0)

# allocate rows to issues
if (last.row==0) {
   
   row.assign <- rep(1L:n.full.row, each = n.col)
   
} else {
   
   row.assign <- c(rep(1L:n.full.row, each = n.col),
                   rep(n.row, last.row))
   
}

# order by number of measurements
n.measured <- 
econ.dat[
   , .N, by = i.vars.label
][
   order(-N)
][
   , N:=NULL
]

# add row assignment
n.measured[, row.num := row.assign]

# merge with data
econ.dat <- merge(econ.dat, n.measured, by = 'i.vars.label')

# creat factor to control plot order
econ.dat[, i.vars.label :=  factor(i.vars.label, 
                                   levels = n.measured[, i.vars.label])
         ]

# save order in which issues are plotted
p.order$econ <- levels(econ.dat$i.vars.label)

plot.list <- lapply(
   1:n.row, function (w) {
      
      if (w!=n.row) {
      
         ggplot(econ.dat[row.num == w],
                   aes(x = year,
                       y = prop.lib,
                       col = subgroup,
                       shape = subgroup)) +
         geom_point(aes(size = subgroup)) +
         geom_line() +
         facet_grid(p.group ~ i.vars.label) +
         gg.opts
         
      } else {
         
         ggplot(econ.dat[row.num == w],
                aes(x = year,
                    y = prop.lib,
                    col = subgroup,
                    shape = subgroup)
         ) +
            geom_point(aes(size = subgroup)) +
            geom_line() +
            facet_grid(p.group ~ i.vars.label) +
            gg.opts +
            theme(
               axis.text.x = element_text(
                  size = 12,
                  margin = margin(c(2, 0, 0, 0))
               ),
               axis.ticks.x = element_line()
            )
      }
   }
)

pdf(
   paste0(graphics.path, 'Figure3.pdf'), 
   width = col.width * n.col,
   height = h.scale * col.width * n.row
)

print(
   plot_grid(plotlist = plot.list,
             nrow = n.row,
             align = 'hv')
)

dev.off()


## Civil Rights Variables -----------------------------------------------------

message('Plotting Civil Rights Issues ...')

# extract vars
cr.dat <- plot.dat[i.vars.class == 2]

# list of civil rights issues
issues <- cr.dat$i.vars.label %>% unique
# number of issues
n.issues <- length(issues)

# number of cols & rows to plot
n.col <- 6L
n.full.row <- floor(n.issues / n.col)
last.row <- n.issues %% n.col
n.row <- n.full.row + as.numeric(last.row > 0)

# allocate rows to issues
if (last.row == 0) {
   
   row.assign <- rep(1L:n.full.row, each = n.col)
   
} else {
   
   row.assign <- c(rep(1L:n.full.row, each = n.col),
                   rep(n.row, last.row))
   
}

# order by number of measurements
n.measured <- cr.dat[
   , .N, by = i.vars.label
][
   order(-N)
][
   , N:=NULL
]

# add row assignment
n.measured[, row.num := row.assign]

# merge with data
cr.dat <- merge(cr.dat,
                n.measured,
                by = 'i.vars.label')

# creat factor to control plot order
cr.dat[, i.vars.label :=
          factor(i.vars.label,
                 levels = n.measured[, i.vars.label])]

# save plotting order
p.order$cr <- levels(cr.dat$i.vars.label)

plot.list <- lapply(
   1:n.row, function (w) {
      
      if (w != n.row) {
         
         g <- ggplot(cr.dat[row.num == w],
                aes(x = year,
                    y = prop.lib,
                    col = subgroup,
                    shape = subgroup)) +
            geom_point(aes(size = subgroup)) +
            geom_line() +
            facet_grid(p.group ~ i.vars.label) +
            gg.opts
         
         return(g)
         
      } else {
         
         if (last.row==0) {
            
            g <- ggplot(cr.dat[row.num == w],
                   aes(x = year,
                       y = prop.lib,
                       col = subgroup,
                       shape = subgroup)) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label) +
               gg.opts +
               theme(
                  axis.text.x=element_text(
                     size=12,
                     margin=margin(c(2,0,0,0))
                  ),
                  axis.ticks.x=element_line()
               )
            
            return(g)
            
         } else {
            
            # copy data 
            tmp <- cr.dat[row.num == w]
            # fill columns with empty facets
            # note: this is done by artificially creating
            #       factor levels and, then, using the
            #       drop=F option in facet_grid
            nn <- unique(tmp$i.vars.label) %>% as.character
            tmp[, i.vars.label := 
                   factor(
                     i.vars.label,
                     levels = c(nn, 1:(n.col - last.row))
                   )]
            # initial plot
            g <- ggplot(tmp,
                        aes(x = year,
                            y = prop.lib,
                            col = subgroup,
                            shape = subgroup
                        )
               ) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label, drop = F) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ),
                  axis.ticks.x = element_line()
               )
            
            # drop artificially created facets (while keeping layout)
            g <- drop.empty.facets(g, n.col, last.row)
            
            # return grob object
            return(g)
         }   
      }
   }
)

pdf(
   paste0(graphics.path,
          'Figure4.pdf'),
   width = col.width * n.col,
   height = h.scale * col.width * n.row
)

print(
   plot_grid(
      plotlist = plot.list,
      nrow = n.row,
      align = 'hv'
   )
)

dev.off()



## Moral Variables ------------------------------------------------------------

message('Plotting Moral Issues (Subset) ...')

# extract vars
m.dat <- plot.dat[i.vars.class == 3]

# order by number of measurements
n.measured <- m.dat[, .N, by = list(i.vars.label, data.source)]
n.measured[, data.source :=
              factor(data.source,
                     levels = c('NES',
                                'GSS'))]

# read in codebook
m.in.out <- read.xlsx(
   paste0(wd.base, 'codebook/variable_summary_final.xlsx'), 
   sheetName='Subset_Moral_Issues',
   stringsAsFactors=F
) %>% 
   data.table %>%
   mutate(in_out = as.logical(in_out)) %>%
   setnames(tolower(names(.)))

# merge information to dataset
n.measured <- merge(n.measured, 
                    m.in.out, 
                    by.x = c('i.vars.label','data.source'),
                    by.y = c('variable', 'data.source'),
                    all.x = T)
m.dat <- merge(m.dat,
               n.measured,
               by = c('i.vars.label', 'data.source'),
               all.x = T)


# select issues and define factor levels
# m.in.out <- m.in.out[
#    category %in% c('gender', 'sex.n.reprod', 'gay', 'other') & in_out == T
# ][
#    , i.vars.label := 
#      factor(Variable, levels = n.measured[, i.vars.label])
# ]

# merge with main dataset
# m.dat <- merge(m.dat,
#                  m.in.out[, c('i.vars.label', 'category')],
#                  by = 'i.vars.label',
#                  all.x = T)

# select categories to plot and assign row numbers
m.dat[, category := 
           factor(category,
                  levels = c('gender',
                             'sex.n.reprod',
                             'gay',
                             'other',
                             'generic.n.rel')
                  )]

m.dat[, row.num :=
           ifelse(category == 'gender',
                  1,
                  ifelse(category == 'sex.n.reprod',
                         2,
                         ifelse(category == 'gay',
                                3, 
                                ifelse(category == 'other',
                                       4, 5))))]
# plotting structure
n.col <- 6L
n.row <- 4L
last.row <- 0L

# plotting order
new.order <- unique(
   m.dat[order(!in_out,row.num,-data.source,-N), i.vars.label]
)
new.order <- new.order[c(1:18,21,19,20,22:length(new.order))]

# creat factor to control plot order
m.dat[, i.vars.label:= factor(i.vars.label,levels=new.order)]

# use subset
m.dat.2 <- m.dat[in_out == T & row.num <= n.row]

# save plotting order
p.order$moral.sub <- new.order[1:(n.row * n.col)]

plot.list <- lapply(
   1:n.row, function (w) {
      
      if (w != n.row) {
         
         ggplot(m.dat.2[row.num == w],
                aes(
                   x = year,
                   y = prop.lib,
                   col = subgroup,
                   shape = subgroup,
                   fill = data.source
                )) +
            geom_rect(aes(
               xmin = -Inf,
               xmax = Inf,
               ymin = -Inf,
               max = Inf
            )) +
            geom_point(aes(size = subgroup)) +
            geom_line() +
            facet_grid(p.group ~ i.vars.label) +
            gg.opts +
            scale_fill_manual(values = c('GSS' = 'grey90',
                                         'NES' = 'white'))
         
      } else {
         
         if (last.row == 0) {
            ggplot(
               m.dat.2[row.num == w],
               aes(
                  x = year,
                  y = prop.lib,
                  col = subgroup,
                  shape = subgroup,
                  fill = data.source
               )
            ) +
               geom_rect(aes(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = -Inf,
                  max = Inf
               )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ),
                  axis.ticks.x = element_line()
               ) +
               scale_fill_manual(values = c('GSS' = 'grey90',
                                            'NES' = 'white'))
            
         } else {
            
            # copy data 
            tmp <- m.dat.2[row.num == w]
            # fill columns with empty facets
            # note: this is done by artificially creating
            #       factor levels and, then, using the
            #       drop=F option in facet_grid
            nn <- unique(tmp$i.vars.label) %>% as.character
            tmp[, i.vars.label := 
                   factor(i.vars.label,
                          levels = c(nn,
                                     1:(n.col - last.row)))]
            # initial plot
            g <- ggplot(tmp,
                        aes(
                           x = year,
                           y = prop.lib,
                           col = subgroup,
                           shape = subgroup,
                           fill = data.source
                        )) +
               geom_rect(aes(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = -Inf,
                  max = Inf
               )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label, drop = F) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ),
                  axis.ticks.x = element_line()) +
               scale_fill_manual(values = c('GSS' = 'grey90',
                                            'NES' = 'white'))
            
            # drop artificially created facets (while keeping layout)
            g <- drop.empty.facets(g, n.col, last.row)
            
            # return grob object
            return(g)
         }   
      }
   }
)

pdf(
   paste0(graphics.path,
          'Figure5.pdf'),
   width = col.width * n.col,
   height = h.scale * col.width * n.row
)

print(
   plot_grid(plotlist = plot.list,
             nrow = n.row,
             align = 'hv'
   )
)

dev.off()


### Full set of issues (too many!)


message('Plotting Moral Issues (All Issues) ...')


# list of moral issues
issues <- m.dat$i.vars.label %>% unique
# number of issues
n.issues <- length(issues)

# number of cols & rows to plot
n.col <- 6L
n.full.row <- floor(n.issues / n.col)
last.row <- n.issues %% n.col
n.row <- n.full.row + as.numeric(last.row > 0)

# drop old row number
m.dat[, row.num := NULL]

# allocate rows to issues
if (last.row==0) {
   
   row.assign <- rep(1L:n.full.row, each = n.col)
   
} else {
   
   row.assign <- c(rep(1L:n.full.row, each = n.col),
                   rep(n.row, last.row))
   
}

# add row number
m.dat <- merge(m.dat,
               data.table(
                  i.vars.label = factor(levels(m.dat$i.vars.label)),
                  row.num = row.assign
               ),
               by = 'i.vars.label',
               all.x = T)


# save plotting order
p.order$moral <- levels(m.dat$i.vars.label)

plot.list <- lapply(
   1:n.row, function (w) {
      
      if (w!=n.row) {
         
         ggplot(m.dat[row.num == w],
                aes(
                   x = year,
                   y = prop.lib,
                   col = subgroup,
                   shape = subgroup,
                   fill = data.source
                )) +
            geom_rect(aes(
               xmin = -Inf,
               xmax = Inf,
               ymin = -Inf,
               max = Inf
            )) +
            geom_point(aes(size = subgroup)) +
            geom_line() +
            facet_grid(p.group ~ i.vars.label) +
            gg.opts +
            scale_fill_manual(values = c('GSS' = 'grey90',
                                         'NES' = 'white'))
         
      } else {
         
         if (last.row == 0) {
            
            ggplot(m.dat[row.num == w],
                   aes(
                      x = year,
                      y = prop.lib,
                      col = subgroup,
                      shape = subgroup,
                      fill = data.source
                   )) +
               geom_rect(aes(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = -Inf,
                  max = Inf
               )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ), 
                  axis.ticks.x = element_line()
               ) +
               scale_fill_manual(values = c('GSS' = 'grey90',
                                            'NES' = 'white'))
            
         } else {
            
            # copy data 
            tmp <- m.dat[row.num == w]
            # fill columns with empty facets
            # note: this is done by artificially creating
            #       factor levels and, then, using the
            #       drop=F option in facet_grid
            nn <- unique(tmp$i.vars.label) %>% as.character
            tmp[, i.vars.label := factor(i.vars.label,
                                         levels=c(
                                         nn,
                                         1:(n.col-last.row))
            )]
            # initial plot
            g <- ggplot(tmp,
                        aes(
                           x = year,
                           y = prop.lib,
                           col = subgroup,
                           shape = subgroup,
                           fill = data.source
                        )) +
               geom_rect(aes(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = -Inf,
                  max = Inf
               )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label, drop = F) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ), 
                  axis.ticks.x = element_line()
               ) +
               scale_fill_manual(values = c('GSS' = 'grey90',
                                            'NES' = 'white'))
            
            # drop artificially created facets (while keeping layout)
            g <- drop.empty.facets(g, n.col, last.row)
            
            # return grob object
            return(g)
         }   
      }
   }
)

pdf(
   paste0(graphics.path,
          'Figure5_all_issues.pdf'),
   width = col.width * n.col,
   height = h.scale * col.width * n.row
)

print(
   plot_grid(plotlist = plot.list,
             nrow = n.row,
             align = 'hv'
   )
)

dev.off()



## Foreign Policy / Security --------------------------------------------------

message('Plotting Foreign Policy / Security Issues ...')

# extract vars
f.dat <- plot.dat[i.vars.class == 4]

# list of issues
issues <- f.dat$i.vars.label %>% unique
# number of issues
n.issues <- length(issues)

# number of cols & rows to plot
n.col <- 6L
n.full.row <- floor(n.issues / n.col)
last.row <- n.issues %% n.col
n.row <- n.full.row + as.numeric(last.row > 0)

# allocate rows to issues
if (last.row==0) {
   
   row.assign <- rep(1L:n.full.row, each = n.col)
   
} else {
   
   row.assign <- c(rep(1L:n.full.row, each = n.col),
                   rep(n.row, last.row))
   
}

# order by number of measurements
n.measured <- f.dat[
   , .N, by = i.vars.label
][
   order(-N)
][
   , N := NULL
]

# add row assignment
n.measured[, row.num := row.assign]

# merge with data
f.dat <- merge(f.dat,
               n.measured,
               by = 'i.vars.label')

# creat factor to control plot order
f.dat[, i.vars.label :=
         factor(i.vars.label,
                levels = n.measured[, i.vars.label])]

# save plotting order
p.order$foreign <- levels(f.dat$i.vars.label)

# save plotting into file
saveRDS(p.order, paste0(data.path, 'p.order.rds'))

plot.list <- lapply(
   1:n.row, function (w) {
      
      if (w != n.row) {
         
         ggplot(f.dat[row.num == w],
                aes(
                   x = year,
                   y = prop.lib,
                   col = subgroup,
                   shape = subgroup,
                   fill = data.source
                )) +
            geom_rect(aes(
               xmin = -Inf,
               xmax = Inf,
               ymin = -Inf,
               max = Inf
            )) +
            geom_point(aes(size = subgroup)) +
            geom_line() +
            facet_grid(p.group ~ i.vars.label) +
            gg.opts +
            scale_fill_manual(values = c('GSS' = 'grey90',
                                         'NES' = 'white'))
         
      } else {
         
         if (last.row==0) {
            
            ggplot(f.dat[row.num == w],
                   aes(
                      x = year,
                      y = prop.lib,
                      col = subgroup,
                      shape = subgroup
                   )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label) +
               gg.opts +
               theme(
                  axis.text.x = element_text(
                     size = 12,
                     margin = margin(c(2, 0, 0, 0))
                  ),
                  axis.ticks.x = element_line()
               )
            
         } else {
            
            # copy data 
            tmp <- f.dat[row.num == w]
            # fill columns with empty facets
            # note: this is done by artificially creating
            #       factor levels and, then, using the
            #       drop=F option in facet_grid
            nn <- unique(tmp$i.vars.label) %>% as.character
            tmp[,i.vars.label:=
                   factor(i.vars.label,
                          levels = c(nn, 1:(n.col - last.row)))]
            # initial plot
            g <- ggplot(tmp,
                        aes(
                           x = year,
                           y = prop.lib,
                           col = subgroup,
                           shape = subgroup
                        )) +
               geom_point(aes(size = subgroup)) +
               geom_line() +
               facet_grid(p.group ~ i.vars.label, drop = F) +
               gg.opts +
               theme(
                  axis.text.x=element_text(
                     size=12,
                     margin=margin(c(2,0,0,0))
                  ),
                  axis.ticks.x=element_line()
               )
            
            # drop artificially created facets (while keeping layout)
            g <- drop.empty.facets(g, n.col, last.row)
            
            # return grob object
            return(g)
            
         }   
      }
   }
)

pdf(
   paste0(graphics.path,
          'Figure6.pdf'),
   width = col.width * n.col,
   height = h.scale * col.width * n.row
)

print(
   plot_grid(plotlist = plot.list,
             nrow = n.row,
             align = 'hv')
)

dev.off()

message('Done!\n')

#### END OF CODE ###