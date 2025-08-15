# setup -------------------------------------------------------------------
# 1---source functions
source('Functions/packFontHandler.R')

# 2---call/install packages and reconcile fonts
## function can be modified @ Scripts/Functions/packHandler.R
packFontHandler()

# data steps --------------------------------------------------------------
## Not run:
# 1---create 'notin' operator
# '%notin%' <- Negate(
#   '%in%'
#   )
# End(**Not run**)

# 2---define vector for wild fish
target.wild <- c(
  'ROSA',
  'JDAC',
  'YAKS',
  'JDAS',
  'AGCW',
  'AGWS',
  'EMCR',
  'EMWS'
)

## Not run:
# 3---define vector for wild and hatchery stocks
# target.tot <- c(
#   'AGGR',
#   'AGGA',
#   'AGGB'
# )
# End(**Not run**)

# 4---set target grouping (should be one of the above: 'target.wild' or 'target.tot')
targ.grp <- 'target.wild'

# 5---read-in and manipulate data
sar.meta.dat <- readRDS(
  'Data/sarMeta.rds'
) |> 
  filter(
    css.grp
    %in%
      !! sym(targ.grp)
  ) |>
  filter(
    mig.yr >= 1994
  ) |> 
  ## add fields for adult returns, row id and distance to CRM
  mutate(
    cases = (sar.est/100)*juv.pop,
    obs.id = 1:n(),
    dist_rkm = ifelse(
      css.grp == 'EMCR' | css.grp == 'EMWS',
      529,
      ifelse(
        css.grp == 'AGCW' | css.grp == 'AGWS',
        461,
        ifelse(
          css.grp == 'ROSA' | css.grp == 'YAKS',
          236,
          ifelse(
            css.grp == 'JDAC' | css.grp == 'JDAS',
            113,
            NA
          )
        )
      )
    )
  ) |>  
  group_by(
    css.grp
  ) |> 
  mutate(
    es.id = seq_along(css.grp)
  )

# 6---calculate summary effect sizes
sar.meta.es <- escalc (
  xi = cases, # adult returns
  ni = juv.pop, # juvenile population
  data = sar.meta.dat , # specify data (from above)
  measure = 'PLO',  # data transformation (logit in this case)
  add = 1/100,  # add constant to account for '0' cell entries
  to = 'only0' # specify which records to apply constant to
)

# 7---generate data summary table
table.1 <- sar.meta.es |> 
  dplyr::select(
    zone,
    # css.grp,
    sar.reach,
    spp.code
  ) |>  
  unique() |>  
  mutate(
    zone=replace(zone, zone=='SNAK', 'Snake R.'),
    zone=replace(zone, zone=='MCOL', 'Mid. Col. R'),
    zone=replace(zone, zone=='UCOL', 'Upp. Col. R.'),
    zone=replace(zone, zone=='UCOL', 'Upp. Col. R.'),
    spp.code = replace(spp.code, spp.code=='CH', 'Chinook'),
    spp.code = replace(spp.code, spp.code=='ST', 'steelhead')
  ) |>  
  mutate(
    riv = c(
      rep('Snake R.',2),
      rep('Yakima R.',2),
      rep('John Day R.',2),
      rep('Entiat and Methow R.', 2)
    )
  ) |>  
  mutate(
    n = c(
      nrow(filter(sar.meta.es, css.grp=='AGCW')),
      nrow(filter(sar.meta.es, css.grp=='AGWS')),
      nrow(filter(sar.meta.es, css.grp=='ROSA')),
      nrow(filter(sar.meta.es, css.grp=='YAKS')),
      nrow(filter(sar.meta.es, css.grp=='JDAC')),
      nrow(filter(sar.meta.es, css.grp=='JDAS')),
      nrow(filter(sar.meta.es, css.grp=='EMCR')),
      nrow(filter(sar.meta.es, css.grp=='EMWS'))
    )
  ) |>
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    zone  = 'Zone',
    # css.grp = 'CSS group ID',
    spp.code = 'Species',
    riv = 'Origin',
    sar.reach = 'SAR est. reach',
    n = 'No. estimates'
  ) |> 
  set_formatter(
    n = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f', big.mark = ','))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  width(
    j = c(
      1,
      3,
      5
    ), 
    1.1
  ) |>  
  width(
    j = c(
      2
    ),
    3.0
  ) |> 
  width(
    j = c(
      4
    ),
    1.7
  ) |>  
  mk_par( 
    j = 2,
    i = 1:2,
    value = as_paragraph(
      'Lwr. Granite',
      as_sub('juv.'),
      '\u2013',
      'Bonn.',
      as_sub('ad.')
    )
  ) |> 
  mk_par( 
    j = 2,
    i = 3:4,
    value = as_paragraph(
      'McNary',
      as_sub('juv.'),
      '\u2013',
      'Bonn.',
      as_sub('ad.')
    )
  ) |>  
  mk_par( 
    j = 2,
    i = 5:6,
    value = as_paragraph(
      'John Day',
      as_sub('juv.'),
      '\u2013',
      'Bonn.',
      as_sub('ad.')
    )
  ) |> 
  mk_par( 
    j = 2,
    i = 7:8,
    value = as_paragraph(
      'Rocky Reach',
      as_sub('juv.'),
      '\u2013',
      'Bonn.',
      as_sub('ad.')
    )
  ) |>
  print()

# specify models ----------------------------------------------------------
# 1---assess basic model structural components
# a---fit models
# i---model 0 -- basic multi-level model (i.e., null model):
## intercept = TRUE;
## random effects = NA; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod0 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

# ii---model 1:
## intercept = TRUE; 
## random effect(s) = group-level; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod1 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp
  ), 
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

# iii---model 2:
## intercept = TRUE;
## random effects = observation-level within css.grp; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod2 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp/obs.id
  ), 
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

# iv---model 3:
## intercept = TRUE;
## random effects = observation-level within css.grp; migration year; 
## fixed effects = NA;
## var-cov matrix = unstructured
mod3 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp/obs.id,
    ~ 1 | mig.yr
  ),
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

# v---model 4:
## intercept = TRUE;
## random effects = observation-level within css.grp; mig. yr. within spp. 
## fixed effects = NA;
## var-cov matrix = unstructured
mod4 <- rma.mv(
  yi = yi, 
  V = vi,
  slab = css.grp,
  data = sar.meta.es,
  random = list(
    ~ 1 | css.grp/obs.id,
    ~ 1 | spp.code/mig.yr
  ),
  test = 't',
  dfs = 'contain',
  method = 'ML'
)

# b---create table of overall (structural) model comparisons
# i---manipulate data
bsFitStats <- fitstats(
  mod0,
  mod1,
  mod2,
  mod3,
  mod4
) |> 
  t() |> 
  as.data.frame()

table.2 <- bsFitStats |> 
  dplyr::select('logLik:',
                'AICc:'
  ) |>  
  rename(
    log.likliehood = 1,
    aicc = 2
  ) |> 
  mutate(
    'Intercept' = c('+', '+','+','+','+'),
    'Fixed.Effects' = c('-','-','-','-','-'),
    'Random.Effects' = c('-','1|css. grp.','1|css.grp/obs.id','1|css.grp/obs.id; 1|mig. yr.', '1|css.grp/obs.id; 1|spp./mig.yr')
  ) |> 
  arrange(
    aicc
  ) |>  
  mutate(
    delta.AICc = round(
      akaike.weights(aicc)$deltaAIC,
      0
    ),
    AICc.weights = round(
      akaike.weights(aicc)$weights,
      2
    ),
    no.param = arrange(
      AIC.rma(
        mod0,
        mod1,
        mod2,
        mod3,
        mod4
      ),
      AIC
    )$df
  ) |> 
  relocate(
    c(
      delta.AICc, 
      AICc.weights
    ),
    .after = aicc
  ) |>  
  relocate(
    c(
      Intercept,
      Fixed.Effects,
      Random.Effects,
      no.param
      
    ),
    .before = log.likliehood
  ) |> 
  # ii---generate output table
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    Intercept = 'Intercept',
    Fixed.Effects = 'Fixed Effects',
    Random.Effects = 'Random Effects',
    no.param = 'No. params.',
    log.likliehood = 'log-liklihood'
  ) |> 
  mk_par( 
    j = 6,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j = 7,
    i = 1,
    value = as_paragraph(
      paste0('\u394','AIC'),
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j =8,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c'),
      ' wt.'
    ),
    part = 'header'
  ) |>
  set_formatter(
    log.likliehood = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f', big.mark = ',')),
    aicc = function(x) ifelse(is.na(x),'', formatC(x,digits = 1, format = 'f', big.mark = ',')),
    delta.AICc = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f', big.mark = ',')),
    AICc.weights = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f', big.mark = ','))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  width(
    j = 2, 
    1.1
  ) |> 
  width(
    j = 3, 
    2.2
  ) |> 
  width(
    j = 4, 
    1.0
  ) |> 
  print()

# hypothesis testing ------------------------------------------------------
#1---multi-model inference
## helper functions necessary to coordinate metafor and MuMin
# eval(metafor:::.MuMIn)

#a---fit full model
## update best supported model from above, with moderators
  top.mod <- bsFitStats |> 
    arrange(
      `AICc:`
    ) |> 
    head(1) |> 
    rownames_to_column() |>  
    dplyr::select(
      rowname
    ) |>
    as.character()

fullMod <- update(
  eval(
    parse(
      text = top.mod
      )
    ),
  ~ wtt + pitph
)

## Not run:
#b---estimate variance inflation factors
#i---calculate VIF in closed-form and simulate props < vif
# table.3 <- vif.rma(
#   fullMod),
#   sim = TRUE,
#   parallel = 'snow',
#   ncpus = detectCores(),
#   seed = 1234
# ) |> 
#   #ii---manipulated data
#   as.data.frame() |> 
#   mutate(
#     mods = c(
#       'WTT',
#       'PITPH'
#     )
#   ) |> 
#   dplyr::select(
#     mods,
#     vif,
#     prop
#   ) |>
#   #iii---generate table
#   flextable() |> 
#   align(
#     i = 1,
#     j = 1,
#     align = 'center',
#     part =  'header'
#   ) |>
#   align(
#     align = 'center',
#     part = 'all'
#   ) |>
#   set_header_labels(
#     mods = 'Moderator',
#     vif = 'VIF'
#   ) |>
#   mk_par( 
#     j =3,
#     i = 1,
#     value = as_paragraph(
#       'Prop.',
#       as_sub('sims.'),
#       ' < VIF'
#     ),
#     part = 'header'
#   ) |>
#   set_formatter(
#     mods = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
#     vif = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
#     prop = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f'))
#   ) |> 
#   fontsize(
#     size = 12,
#     part = 'all'
#   ) |>
#   flextable::font(
#     fontname = 'Times New Roman',
#     part = 'all'
#   ) |>
#   border_remove() |>
#   hline(
#     i=1,
#     part='header',
#     border = fp_border(
#       color='black',
#       width = 1
#     )
#   ) |>
#   hline_top(
#     part='header',
#     border = fp_border(
#       color='black',
#       width = 2
#     )
#   ) |>
#   hline_bottom(
#     part='body',
#     border = fp_border(
#       color='black',
#       width = 2
#     )
#   ) |>
#   autofit() |> 
#   print()

#iv---output table
# save_as_image(
#   'Output/Tables/sarMetaTbl3.png',
#   res = 2000
# )
# End(**Not run**)

# c---conduct MMI and create and output summary table
table.4 <- dredge(
  fullMod,
  trace=2
) |>
  as.data.frame() |> 
  # i---generate output table
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    `(Intercept)` = 'Intercept',
    # `factor(spp.code)` = 'Spp.',
    pitph = 'PITPH',
    wtt = 'WTT',
    df = 'No. params.',
    logLik = 'log-liklihood'
  ) |>
  mk_par( 
    j = 6,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j = 7,
    i = 1,
    value = as_paragraph(
      paste0('\u394','AIC'),
      as_sub('c')
    ),
    part = 'header'
  ) |> 
  mk_par( 
    j = 8,
    i = 1,
    value = as_paragraph(
      'AIC',
      as_sub('c'),
      ' wt.'
    ),
    part = 'header'
  ) |>
  set_formatter(
    # `factor(spp.code)` = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    pitph = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    wtt = function(x) ifelse(is.na(x),'', formatC(x,digits = 3, format = 'f')),
    df = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f')),
    AICc = function(x) ifelse(is.na(x),'', formatC(x,digits = 1, format = 'f')),
    delta = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f')),
    weight = function(x) ifelse(is.na(x),'', formatC(x,digits = 2, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  width(
    j = 4, 
    0.95
  ) |>
  width(
    j = 5, 
    1.05
  ) |>
  print()

# d---specify best model
# i---update full model based on model selection table
bestMod <- update(
  fullMod,
  ~ wtt + pitph
)

# e---plot model-averaged and best model coefficients and CIs
# i---estimate model-averaged coefficients
avgMod <- dredge(
  bestMod,
  trace = 3
) |> 
  subset(
    delta <= 4.0 # specify confidence set to include models where dAICc <= 4.0
    # recalc.weights = FALSE
  ) |> 
  model.avg()

#ii---data manipulation
sarMetaParam.plot <- bind_rows(
  data.frame(
    selType = as.factor(rep('best',2)),
    param.lbs = c('WTT',
                  'PITPH'),
    param = bestMod$beta[2:3,],
    lcl = confint(
      bestMod, 
      fixed = TRUE,
      level = 0.90 # specify 90% confidence limits
    ) |> 
      as.data.frame() |> 
      as.data.frame() |> 
      slice(-1) |> 
      dplyr::select(
        ci.lb
      ) |> 
      unlist() |> 
      unname(),
    ucl = confint(
      bestMod, 
      fixed = TRUE,
      level = 0.90 # specify 90% confidence limits
    ) |> 
      as.data.frame() |> 
      as.data.frame() |> 
      slice(-1) |> 
      dplyr::select(
        ci.ub
      ) |> 
      unlist() |> 
      unname()) |> 
    rename(
      Estimate = 3
    ) |> 
    `rownames<-`( NULL ),
  coefTable(
    avgMod, 
    full = TRUE
  ) |>
    as.data.frame() |> 
    dplyr::select(
      Estimate
    ) |> 
    tibble::rownames_to_column('param.lbs') |> 
    left_join(
      confint(
        avgMod, 
        full = TRUE,
        level = 0.90 # specify 90% confidence limits
      ) |> 
        as.data.frame() |> 
        tibble::rownames_to_column('param.lbs'),
      by = 'param.lbs'
    ) |>  
    as.data.frame() |>  
    rename(
      lcl = 3,
      ucl = 4
    ) |>
    mutate(
      param.lbs = c(
        'Intercept',
        'PITPH',
        'WTT'
      )
    ) |> 
    tail(3) |> 
    mutate(
      selType = as.factor(rep('average',3))
    ) |> 
    relocate(
      selType,
      .before = param.lbs
    )
) |> 
  as.data.frame() |> 
  filter(
    param.lbs != 'Intercept'
  ) |> 
  #iii---generate plot
  ggplot(aes(x=Estimate, y = param.lbs, color = selType))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 18,vjust = 1,color = 'black',family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 18,vjust = -1,color = 'black',family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 16,angle = 0,hjust = 0.5,vjust = 0.5,color = 'black',family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 16,color = 'black',family = 'Calibri'),
        axis.ticks.length = unit(2,'mm'),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 16,family = 'Calibri',face = 'bold'))+
  scale_colour_manual(name='Transplanted', values = c('best' = '#D55E00','average' = 'black'), breaks = c('best', 'average')) +
  geom_vline(xintercept = 0,linetype = 'dashed') +
  geom_errorbarh(aes(xmax=ucl,xmin=lcl),height = 0, position = position_dodge(width = -0.3)) +
  geom_point(size = 4, position = position_dodge(width = -0.3)) +
  # geom_point(size = 4,shape = 1, position = position_dodge(width = 0.3)) +
  labs(y = 'Parameter',x = 'Estimate') +
  scale_y_discrete(limits = c('WTT','PITPH')) + 
  scale_x_continuous(limits=c(-0.3,0.3),
                     breaks = seq(-0.3,0.3,0.2),
                     labels = function(x) format(x, scientific = FALSE)
  )

# iv---print figure for review
print(sarMetaParam.plot)

# model verification (average predictions) --------------------------------
# a---specify component models
# i---first component model
predMod1 <- update(
  fullMod,
  ~ wtt + pitph
)

# ii---second component model
predMod2 <- update(
  fullMod,
  ~ wtt
)

# b---estimate BLUPs (best linear unbiased predictions for random effects)
# i---first component model
randEffEsts_mod1 <- ranef(
  predMod1
)

# ii---second component model
randEffEsts_mod2 <- ranef(
  predMod2
)

# b---generate weighted predictions
# i---first component model
# *---generate predictions data set
sarMeta.pred_mod1 <- predict(
  bestMod,
  addx = TRUE
) |> 
  as.data.frame() |>   
  mutate(
    zone = sar.meta.es |>
      dplyr::select(
        zone,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        zone
      ) |> 
      unlist() |> 
      unname(),
    css.grp = sar.meta.es |>
      dplyr::select(
        css.grp,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        css.grp
      ) |> 
      unlist() |> 
      unname(),
    mig.yr = sar.meta.es |>
      dplyr::select(
        mig.yr,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        mig.yr
      ) |> 
      unlist() |> 
      unname(),
    obs.id = sar.meta.es |>
      dplyr::select(
        obs.id,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        obs.id
      ) |> 
      unlist() |> 
      unname(),
    spp.code = sar.meta.es |>
      dplyr::select(
        spp.code,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        spp.code
      ) |> 
      unlist() |> 
      unname(),
    nestGrpObs = paste0(
      sar.meta.es |>
        dplyr::select(
          css.grp,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          css.grp
        ) |> 
        unlist() |> 
        unname(),
      '/',
      sar.meta.es |>
        dplyr::select(
          obs.id,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          obs.id
        ) |> 
        unlist() |> 
        unname()
    ),
    nestSppMY = paste0(
      sar.meta.es |>
        dplyr::select(
          spp.code,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          spp.code
        ) |> 
        unlist() |> 
        unname(),
      '/',
      sar.meta.es |>
        dplyr::select(
          mig.yr,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          mig.yr
        ) |> 
        unlist() |> 
        unname()
    )
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod1$`spp.code/mig.yr`[1], 'nestSppMY') |>  
      as.data.frame(),
    by = 'nestSppMY'
  ) |> 
  rename(
    fnestSppMY = intrcpt
  ) |>
  left_join(
    tibble::rownames_to_column(randEffEsts_mod1$`css.grp/obs.id`[1], 'nestGrpObs') |>  
      as.data.frame(),
    by = 'nestGrpObs'
  ) |>  
  rename(
    fnestGrpObs = intrcpt
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod1$css.grp[1], 'css.grp') |>  
      as.data.frame(),
    by = 'css.grp'
  ) |> 
  rename(
    fcss.grp = intrcpt
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod1$spp.code[1], 'spp.code') |>  
      as.data.frame(),
    by = 'spp.code'
  ) |> 
  rename(
    fspp.code = intrcpt
  ) |> 
  rowwise() |> 
  mutate(
    flogitPred = rowSums(
      pick(
        pred, 
        fnestSppMY, 
        fnestGrpObs, 
        fcss.grp,
        fspp.code), 
      na.rm = FALSE
    )
  ) |>
  mutate(
    arithPred = inv.logit(pred),
    farithPred = inv.logit(flogitPred)
  ) |> 
  mutate(
    zone=replace(zone, zone=='SNAK', 'Snake R.')
  ) |> 
  mutate(
    zone=replace(zone, zone=='MCOL', 'Middle Columbia R.')
  ) |>
  mutate(
    zone=replace(zone, zone=='UCOL', 'Upper Columbia R.')
  ) |>
  mutate(
    across(
      zone,
      factor
    )
  ) |> 
  as.data.frame()

# *---generate model verification data set
modVerif_mod1 <- sarMeta.pred_mod1 |>  
  mutate(
    arithObs = na.omit(sar.meta.es$sar.est)/100,
    spp = sar.meta.es |>
      dplyr::select(
        spp.code,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        spp.code
      ) |> 
      unlist() |> 
      unname(),
    logitObs = logit(na.omit(sar.meta.es$sar.est)/100)
  ) |> 
  mutate(
    spp=replace(spp, spp=='CH', 'Chinook')
  ) |> 
  mutate(
    spp=replace(spp, spp=='ST', 'steelhead')
  ) |> 
  mutate(
    weight = as.numeric(
      avgMod$msTable[5] |> 
        head(1)
    )
  ) |> 
  mutate(
    wt_arithPred = arithPred * weight,
    wt_farithPred = farithPred * weight,
    wt_logitPred = pred * weight,
    wt_flogitPred = flogitPred * weight
  )

# ii---second component model
# *---generate predictions data set
sarMeta.pred_mod2 <- predict(
  update(
    bestMod,
    ~ wtt
  ),
  addx = TRUE
) |> 
  as.data.frame() |>   
  mutate(
    zone = sar.meta.es |>
      dplyr::select(
        zone,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        zone
      ) |> 
      unlist() |> 
      unname(),
    css.grp = sar.meta.es |>
      dplyr::select(
        css.grp,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        css.grp
      ) |> 
      unlist() |> 
      unname(),
    mig.yr = sar.meta.es |>
      dplyr::select(
        mig.yr,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        mig.yr
      ) |> 
      unlist() |> 
      unname(),
    obs.id = sar.meta.es |>
      dplyr::select(
        obs.id,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        obs.id
      ) |> 
      unlist() |> 
      unname(),
    spp.code = sar.meta.es |>
      dplyr::select(
        spp.code,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        spp.code
      ) |> 
      unlist() |> 
      unname(),
    nestGrpObs = paste0(
      sar.meta.es |>
        dplyr::select(
          css.grp,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          css.grp
        ) |> 
        unlist() |> 
        unname(),
      '/',
      sar.meta.es |>
        dplyr::select(
          obs.id,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          obs.id
        ) |> 
        unlist() |> 
        unname()
    ),
    nestSppMY = paste0(
      sar.meta.es |>
        dplyr::select(
          spp.code,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          spp.code
        ) |> 
        unlist() |> 
        unname(),
      '/',
      sar.meta.es |>
        dplyr::select(
          mig.yr,
          cases
        ) |> 
        na.omit() |> 
        dplyr::select(
          mig.yr
        ) |> 
        unlist() |> 
        unname()
    )
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod2$`spp.code/mig.yr`[1], 'nestSppMY') |>  
      as.data.frame(),
    by = 'nestSppMY'
  ) |> 
  rename(
    fnestSppMY = intrcpt
  ) |>
  left_join(
    tibble::rownames_to_column(randEffEsts_mod2$`css.grp/obs.id`[1], 'nestGrpObs') |>  
      as.data.frame(),
    by = 'nestGrpObs'
  ) |>  
  rename(
    fnestGrpObs = intrcpt
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod2$css.grp[1], 'css.grp') |>  
      as.data.frame(),
    by = 'css.grp'
  ) |> 
  rename(
    fcss.grp = intrcpt
  ) |> 
  left_join(
    tibble::rownames_to_column(randEffEsts_mod2$spp.code[1], 'spp.code') |>  
      as.data.frame(),
    by = 'spp.code'
  ) |> 
  rename(
    fspp.code = intrcpt
  ) |> 
  rowwise() |> 
  mutate(
    flogitPred = rowSums(
      pick(
        pred, 
        fnestSppMY, 
        fnestGrpObs, 
        fcss.grp,
        fspp.code), 
      na.rm = FALSE
    )
  ) |>
  mutate(
    arithPred = inv.logit(pred),
    farithPred = inv.logit(flogitPred)
  ) |> 
  mutate(
    zone=replace(zone, zone=='SNAK', 'Snake R.')
  ) |> 
  mutate(
    zone=replace(zone, zone=='MCOL', 'Middle Columbia R.')
  ) |>
  mutate(
    zone=replace(zone, zone=='UCOL', 'Upper Columbia R.')
  ) |>
  mutate(
    across(
      zone,
      factor
    )
  ) |> 
  as.data.frame()

# *---generate model verification data set
modVerif_mod2 <- sarMeta.pred_mod2 |>  
  mutate(
    arithObs = na.omit(sar.meta.es$sar.est)/100,
    spp = sar.meta.es |>
      dplyr::select(
        spp.code,
        cases
      ) |> 
      na.omit() |> 
      dplyr::select(
        spp.code
      ) |> 
      unlist() |> 
      unname(),
    logitObs = logit(na.omit(sar.meta.es$sar.est)/100)
  ) |> 
  mutate(
    spp=replace(spp, spp=='CH', 'Chinook')
  ) |> 
  mutate(
    spp=replace(spp, spp=='ST', 'steelhead')
  ) |> 
  mutate(
    weight = as.numeric(
      avgMod$msTable[5] |> 
        tail(1)
    )
  ) |> 
  mutate(
    wt_arithPred = arithPred * weight,
    wt_farithPred = farithPred * weight,
    wt_logitPred = pred * weight,
    wt_flogitPred = flogitPred * weight
  )

# iii---combine predictions from component models
modVerif_avg <- bind_rows(
  modVerif_mod1 |> 
    dplyr::select(
      wt_arithPred,
      wt_farithPred,
      wt_logitPred,
      wt_flogitPred
    ) |>  
    rownames_to_column(), 
  modVerif_mod2 |>
    dplyr::select(
      wt_arithPred,
      wt_farithPred,
      wt_logitPred,
      wt_flogitPred
    ) |> 
    rownames_to_column()
) |> 
  group_by(rowname) |>
  summarise_all(sum) |> 
  arrange(
    as.numeric(rowname)
  ) |> 
  dplyr::select(
    -rowname
  ) |> 
  mutate(
    zone = modVerif_mod1$zone,
    spp = modVerif_mod1$spp,
    arithObs = modVerif_mod1$arithObs,
    logitObs = modVerif_mod1$logitObs
  ) |> 
  rename(
    arithPred = wt_arithPred,
    farithPred = wt_farithPred,
    logitPred = wt_logitPred,
    flogitPred = wt_flogitPred
  )

# c---model performance
# i---estimate index of agreement
# *---marginal predictions; arithmetic scale
sarMeta.IOAarithFix <- modStats(
  modVerif_avg,
  mod = 'arithPred',
  obs = 'arithObs',
  statistic = c('IOA')
) |>
  as.data.frame() |> 
  dplyr::select(
    IOA
  ) |> 
  round(2) |> 
  as.numeric()

# *---marginal predictions; logit scale
sarMeta.IOAlogitFix <- modStats(
  modVerif_avg,
  mod = 'logitPred',
  obs = 'logitObs',
  statistic = c('IOA')
) |>
  as.data.frame() |> 
  dplyr::select(
    IOA
  ) |> 
  round(2) |> 
  as.numeric()

# *---conditional predictions; arithmetic scale
sarMeta.IOAarithRand <- modStats(
  modVerif_avg,
  mod = 'farithPred',
  obs = 'arithObs',
  statistic = c('IOA')
) |>
  as.data.frame() |> 
  dplyr::select(
    IOA
  ) |> 
  round(2) |> 
  as.numeric()

# *---conditional predictions; logit scale
sarMeta.IOAlogitRand <- modStats(
  modVerif_avg,
  mod = 'flogitPred',
  obs = 'logitObs',
  statistic = c('IOA')
) |>
  as.data.frame() |> 
  dplyr::select(
    IOA
  ) |> 
  round(2) |> 
  as.numeric()

# ii---generate plots
# *---marginal predictions; arithmetic scale
sarMetaVerif.arithFixPlot <- ggplot(
  data = modVerif_avg |> 
    mutate(
      sppCol = paste0(spp,', ',zone)
    ),
  aes(
    x = arithPred, y = arithObs, fill = as.factor(sppCol), color = as.factor(sppCol), alpha = as.factor(sppCol)
  )
) +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 24,vjust = 1,margin = margin(t = 0, r = 40, b = 0, l = 0),family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 24,vjust = -1,margin = margin(t = 4, r = 0, b = 0, l = 0),family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 22,color='black', vjust=0.5,family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 22,color='black',family = 'Calibri'),
        legend.title = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, 'cm'),
        legend.text=element_text(face = 'bold',size = 20,color='black',family = 'Calibri'),
        legend.position = 'inside',
        legend.key.spacing.y = unit(0.2, "cm"),
        legend.position.inside = c(0.47,0.23),
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length = unit(0.15, 'cm'))+
  labs(title ='Marginal Predictions', y = 'Estimated', x = 'Predicted') +
  theme(plot.title = element_text(hjust = 0.5,size = 24,face = 'bold',family = 'Calibri')) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(shape = 21,size = 4.5,stroke=0.5) +
  annotate("text", x = 0.008, y = 0.11, label = bquote(d[r]~ '='~.(sarMeta.IOAarithFix)), size = 8, family = 'Calibri') +
  scale_color_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 'black',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = 'black',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = 'black',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
      )
  ) +
  scale_alpha_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 1.0,
      'steelhead, Snake R.' = 0.7,
      'Chinook, Middle Columbia R.' = 1.0,
      'steelhead, Middle Columbia R.' = 0.7,
      'Chinook, Upper Columbia R.' = 1.0,
      'steelhead, Upper Columbia R.' = 0.7
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_fill_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = '#999999',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = '#E69F00',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = '#56B4E9',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_y_continuous(limits=c(0,0.12),breaks = seq(0,0.12,0.02),labels = scales::percent)+
  scale_x_continuous(limits=c(0,0.12),breaks = seq(0,0.12,0.02),labels = scales::percent)

# *---print figure for review
print(sarMetaVerif.arithFixPlot)

# *---marginal predictions; logit scale
sarMetaVerif.logitFixPlot <- ggplot(
  data = modVerif_avg |> 
    mutate(
      sppCol = paste0(spp,', ',zone)
    ) |> 
    filter(
      logitObs != '-Inf'
    ),
  aes(
    x = logitPred, y = logitObs, fill = as.factor(sppCol), color = as.factor(sppCol), alpha = as.factor(sppCol)
  )
) +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 24,vjust = 1,margin = margin(t = 0, r = 40, b = 0, l = 0),family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 24,vjust = -1,margin = margin(t = 4, r = 0, b = 0, l = 0),family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 22,color='black', vjust = 0.5,family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 22,color='black',family = 'Calibri'),
        legend.title = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, 'cm'),
        legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
        legend.position = 'none',
        legend.position.inside = c(0.8,0.4),
        axis.ticks.length = unit(0.15, 'cm'))+
  labs(title ='', y = 'logit(Estimated)', x = 'logit(Predicted)') +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(shape = 21,size = 4.5,stroke=0.5) +
  annotate("text", x = -7.6, y = -1.5, label = bquote(d[r]~ '='~.(sarMeta.IOAlogitFix)), size = 8, family = 'Calibri') +
  scale_color_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 'black',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = 'black',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = 'black',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_alpha_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 1.0,
      'steelhead, Snake R.' = 0.7,
      'Chinook, Middle Columbia R.' = 1.0,
      'steelhead, Middle Columbia R.' = 0.7,
      'Chinook, Upper Columbia R.' = 1.0,
      'steelhead, Upper Columbia R.' = 0.7
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_fill_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = '#999999',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = '#E69F00',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = '#56B4E9',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_y_continuous(limits=c(-8,-1),breaks = seq(-8,-1,1.0))+
  scale_x_continuous(limits=c(-8,-1),breaks = seq(-8,-1,1.0))


# *---print figure for review
print(sarMetaVerif.logitFixPlot)

# *---conditional predictions; arithmetic scale
sarMetaVerif.arithRandPlot <- ggplot(
  data = modVerif_avg |> 
    mutate(
      sppCol = paste0(spp,', ',zone)
    ),
  aes(
    x = farithPred, y = arithObs, fill = as.factor(sppCol), color = as.factor(sppCol), alpha = as.factor(sppCol)
  )
) +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 24,vjust = 1,margin = margin(t = 0, r = 40, b = 0, l = 0),family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 24,vjust = -1,margin = margin(t = 4, r = 0, b = 0, l = 0),family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 22,color='black', vjust=0.5,family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 22,color='black',family = 'Calibri'),
        legend.title = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, 'cm'),
        legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
        legend.position = 'none',
        legend.position.inside = c(0.8,0.4),
        axis.ticks.length = unit(0.15, 'cm'))+
  labs(title ='Conditional Predictions', y = '', x = 'Predicted') +
  theme(plot.title = element_text(hjust = 0.5,size = 24,face = 'bold',family = 'Calibri')) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(shape = 21,size = 4.5,stroke=0.5) +
  annotate("text", x = 0.008, y = 0.11, label = bquote(d[r]~ '='~.(sarMeta.IOAarithRand)), size = 8, family = 'Calibri') +
  scale_color_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 'black',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = 'black',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = 'black',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_alpha_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 1.0,
      'steelhead, Snake R.' = 0.7,
      'Chinook, Middle Columbia R.' = 1.0,
      'steelhead, Middle Columbia R.' = 0.7,
      'Chinook, Upper Columbia R.' = 1.0,
      'steelhead, Upper Columbia R.' = 0.7
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_fill_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = '#999999',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = '#E69F00',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = '#56B4E9',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_y_continuous(limits=c(0,0.12),breaks = seq(0,0.12,0.02),labels = scales::percent)+
  scale_x_continuous(limits=c(0,0.12),breaks = seq(0,0.12,0.02),labels = scales::percent)


# *---print figure for review
print(sarMetaVerif.arithRandPlot)

# *---conditional predictions; logit scale
sarMetaVerif.logitRandPlot <- ggplot(
  data = modVerif_avg |> 
    mutate(
      sppCol = paste0(spp,', ',zone)
    ) |> 
    filter(
      logitObs != '-Inf'
    ),
  aes(
    x = flogitPred, y = logitObs, fill = as.factor(sppCol), color = as.factor(sppCol), alpha = as.factor(sppCol)
  )
) +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 24,vjust = 1,margin = margin(t = 0, r = 40, b = 0, l = 0),family = 'Calibri'),
        axis.title.x = element_text(face = 'bold', size = 24,vjust = -1,margin = margin(t = 4, r = 0, b = 0, l = 0),family = 'Calibri'),
        axis.text.x = element_text(face = 'bold',size = 22,color='black', vjust=0.5,family = 'Calibri'),
        axis.text.y = element_text(face = 'bold',size = 22,color='black',family = 'Calibri'),
        legend.title = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.15, 0.1, 'cm'),
        legend.text=element_text(face = 'bold',size = 18,color='black',family = 'Calibri'),
        legend.position = 'none',
        legend.position.inside = c(0.8,0.4),
        axis.ticks.length = unit(0.15, 'cm'))+
  labs(title ='', y = '', x = 'logit(Predicted)') +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = 'bold',family = 'Calibri')) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(shape = 21,size = 4.5,stroke=0.5) +
  annotate("text", x = -7.6, y = -1.5, label = bquote(d[r]~ '='~.(sarMeta.IOAlogitRand)), size = 8, family = 'Calibri') +
  scale_color_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 'black',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = 'black',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = 'black',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_alpha_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = 1.0,
      'steelhead, Snake R.' = 0.7,
      'Chinook, Middle Columbia R.' = 1.0,
      'steelhead, Middle Columbia R.' = 0.7,
      'Chinook, Upper Columbia R.' = 1.0,
      'steelhead, Upper Columbia R.' = 0.7
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_fill_manual(
    name = '',
    values = c(
      'Chinook, Snake R.' = '#999999',
      'steelhead, Snake R.' = '#999999',
      'Chinook, Middle Columbia R.' = '#E69F00',
      'steelhead, Middle Columbia R.' = '#E69F00',
      'Chinook, Upper Columbia R.' = '#56B4E9',
      'steelhead, Upper Columbia R.' = '#56B4E9'
    ),
    breaks = c(
      'steelhead, Middle Columbia R.',
      'Chinook, Middle Columbia R.',
      'steelhead, Upper Columbia R.',
      'Chinook, Upper Columbia R.',
      'steelhead, Snake R.',
      'Chinook, Snake R.'
    )
  ) +
  scale_y_continuous(limits=c(-8,-1),breaks = seq(-8,-1,1.0))+
  scale_x_continuous(limits=c(-8,-1),breaks = seq(-8,-1,1.0))


# *---print figure for review
print(sarMetaVerif.logitRandPlot)

# *---combined plot
sarMetaVerif.combPlot <- ggarrange(
  sarMetaVerif.arithFixPlot,
  sarMetaVerif.arithRandPlot,
  sarMetaVerif.logitFixPlot,
  sarMetaVerif.logitRandPlot,
  ncol = 2,
  nrow = 2
)

# *---print figure for review
print(sarMetaVerif.combPlot)

# variance components -----------------------------------------------------
# variance components -----------------------------------------------------
#c--- variance component tables
#i--- full model (all moderators)
fullVC.mod <- bestMod

fullVC.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3',
    '\\sigma^2_4'
  ),
  sigEst = fullVC.mod$sigma2,
  sQRtsigEst = sqrt(fullVC.mod$sigma2),
  nLvls = fullVC.mod$s.nlevels,
  fac = fullVC.mod$s.names
) |> 
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'spp. code',
      'spp. code/mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |> 
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |>
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Full model", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |> 
  print()

#ii--- reduced model (no moderators)
redVC_nm.mod <- update(
  fullMod,
  ~ 1
)

redVC_nm.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3',
    '\\sigma^2_4'
  ),
  sigEst = redVC_nm.mod$sigma2,
  sQRtsigEst = sqrt(redVC_nm.mod$sigma2),
  nLvls = redVC_nm.mod$s.nlevels,
  fac = redVC_nm.mod$s.names
) |> 
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'spp. code',
      'spp. code/mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |>
  autofit() |> 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (no moderators)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |> 
  print()

#ii--- reduced model (PITPH only)
redVC_pitph.mod <- update(
  fullMod,
  ~ pitph
)

redVC_pitph.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3',
    '\\sigma^2_4'
  ),
  sigEst = redVC_pitph.mod$sigma2,
  sQRtsigEst = sqrt(redVC_pitph.mod$sigma2),
  nLvls = redVC_pitph.mod$s.nlevels,
  fac = redVC_pitph.mod$s.names
) |> 
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'spp. code',
      'spp. code/mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |>
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (mods. = PITPH)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |>
  print()

#ii--- reduced model (WTT only)
redVC_wtt.mod <- update(
  fullMod,
  ~ wtt
)

redVC_wtt.tbl <- data.frame(
  sigNm = c(
    '\\sigma^2_1',
    '\\sigma^2_2',
    '\\sigma^2_3',
    '\\sigma^2_4'
  ),
  sigEst = redVC_wtt.mod$sigma2,
  sQRtsigEst = sqrt(redVC_wtt.mod$sigma2),
  nLvls = redVC_wtt.mod$s.nlevels,
  fac = redVC_wtt.mod$s.names
) |>  
  mutate(
    fac = c(
      'CSS grp.',
      'CSS grp./obs.',
      'spp. code',
      'spp. code/mig. yr.'
    )
  ) |> 
  flextable() |>  
  align(
    i = 1,
    j = 1,
    align = 'center',
    part =  'header'
  ) |> 
  align(
    align = 'center',
    part = 'all'
  ) |> 
  set_header_labels(
    sigNm = 'Parameter',
    sigEst = 'Estimate',
    sQRtsigEst = 'Sqr. Rt. Estimate',
    nLvls = 'No. Levels',
    fac = 'Factor'
  ) |> 
  set_formatter(
    sigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    sQRtsigEst = function(x) ifelse(is.na(x),'', formatC(x,digits = 4, format = 'f')),
    nLvls = function(x) ifelse(is.na(x),'', formatC(x,digits = 0, format = 'f'))
  ) |>  
  fontsize(
    size = 12,
    part = 'all'
  ) |> 
  flextable::font(
    fontname = 'Times New Roman',
    part = 'all'
  ) |> 
  border_remove() |> 
  hline(
    i=1,
    part='header',
    border = fp_border(
      color='black',
      width = 1
    )
  ) |> 
  hline_top(
    part='header',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |> 
  hline_bottom(
    part='body',
    border = fp_border(
      color='black',
      width = 2
    )
  ) |>  
  mk_par(
    j = 'sigNm',
    value = as_paragraph(as_equation(sigNm))
  ) |> 
  autofit() |> 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Reduced model (mods. = WTT)", 
        props = fp_text_default(
          bold = TRUE, 
          font.family = 'Times New Roman', 
          font.size = 14
        )
      )
    ),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fp_par(text.align = "left", padding = 3)
  ) |>
  print()

# simulations -------------------------------------------------------------
# 1---specify simulation parameters
# a---species
# *---0 = Chinook
# *---1 = steelhead
sppNum <- 1

# b---number of replications
reps <- 100000

# 2---estimate distribution (normal) parameters for random effects
# a---first component model
# *---observation-level random effect
paramObs_mod1 <- enorm(
  method = 'mle',
  randEffEsts_mod1$`css.grp/obs.id` |>
    pull(intrcpt)
)

# *---steelhead migration year random effect
paramSTLmy_mod1 <- enorm(
  method = 'mle',
  randEffEsts_mod1$`spp.code/mig.yr` |>
    tibble::rownames_to_column() |>
    filter(
      grepl('ST', rowname)
    ) |>
    pull(intrcpt)
)

# *---Chinook migration year random effect
paramCHNmy_mod1 <- enorm(
  method = 'mle',
  randEffEsts_mod1$`spp.code/mig.yr` |>
    tibble::rownames_to_column() |>
    filter(
      grepl('CH', rowname)
    ) |>
    pull(intrcpt)
)

# *---CSS group random effect
paramCSSgrp_mod1 <- enorm(
  method = 'mle',
  randEffEsts_mod1$css.grp |>
    pull(intrcpt)
)

# b---second component model
# *---observation-level random effect
paramObs_mod2 <- enorm(
  method = 'mle',
  randEffEsts_mod2$`css.grp/obs.id` |>
    pull(intrcpt) 
)

# *---steelhead migration year random effect
paramSTLmy_mod2 <- enorm(
  method = 'mle',
  randEffEsts_mod2$`spp.code/mig.yr` |> 
    tibble::rownames_to_column() |> 
    filter(
      grepl('ST', rowname)
    ) |>
    pull(intrcpt) 
)

# *---Chinook migration year random effect 
paramCHNmy_mod2 <- enorm(
  method = 'mle',
  randEffEsts_mod2$`spp.code/mig.yr` |> 
    tibble::rownames_to_column() |> 
    filter(
      grepl('CH', rowname)
    ) |>
    pull(intrcpt) 
)

# *---CSS group random effect 
paramCSSgrp_mod2 <- enorm(
  method = 'mle',
  randEffEsts_mod2$css.grp |>
    pull(intrcpt) 
)

# 3---simulation routines
# a---first component model
# i---generate predictions from fixed effects
fixPredFun_mod1 <- function(sppNum,i){
  sarMeta.pred <- predict(
    predMod1,
    newmods = cbind(c(seq(2,20,1)),c(rep(i,19))),
    # transf = transf.ilogit,
    addx = TRUE
  ) |> 
    as.data.frame()
  # print(sarMeta.pred)
}

# ii---create output data frame to store replications
fp.out.preds_mod1 <- lapply(
  seq(0.5,3,0.5),
  function(i){
    cbind(c(seq(2,20,1)),c(rep(i,19)))
  }
)

fp.out.preds_mod1 <- do.call(
  rbind,
  fp.out.preds_mod1
) |> 
  as.data.frame()

# iii---apply function across a range of PITPH values
# *---generate predictions based on fixed effects
outFun_mod1 <- function(){
  fp.out_mod1 <- lapply(
    seq(0.5,3,0.5), 
    fixPredFun_mod1,
    sppNum = sppNum
  ) |> 
    bind_rows(.id = 'column_label')
  
# *---add-in draws from simulated distributions (random-normal) of random effects
  fp.out2_mod1 <- fp.out_mod1 |> 
    mutate(
      fObs.id_mod1 = rnorm(
        nrow(fp.out_mod1),
        mean = paramObs_mod1$parameters[1],
        sd = paramObs_mod1$parameters[2]
      ),
      fMig.yr_mod1 = rnorm(
        nrow(fp.out_mod1), 
        mean = if_else(sppNum == 1, paramSTLmy_mod1$parameters[1], paramCHNmy_mod1$parameters[1]), 
        sd = if_else(sppNum == 1, paramSTLmy_mod1$parameters[2], paramCHNmy_mod1$parameters[2])
      ),
      fspp.code_mod1 = if_else(
        sppNum == 1, 
        as.numeric(
          randEffEsts_mod1$spp.code |>
            pull(intrcpt) |> 
            tail(1)
        ),
        as.numeric(
          randEffEsts_mod1$spp.code |>
            pull(intrcpt) |> 
            head(1)
        )
      ),
      fCSS.grp_mod1 = rnorm(
        nrow(fp.out_mod1), 
        mean = paramCSSgrp_mod1$parameters[1], 
        sd = paramCSSgrp_mod1$parameters[2]
      )
    ) |> 
    rowwise() |>
    mutate(
      logitPred_mod1 = rowSums(
        pick(
          pred, 
          fObs.id_mod1, 
          fMig.yr_mod1,
          fspp.code_mod1,
          fCSS.grp_mod1), 
        na.rm = FALSE
      )
    ) |>
    # *---back-transform predictions
    mutate(
      arithPred_mod1 = inv.logit(logitPred_mod1)
    ) |> 
    as.data.frame()
  
  # *---combine data into final (mean) predictions data frame  
  fp.out.preds_mod1 <<- cbind(
    fp.out.preds_mod1,
    fp.out2_mod1[,ncol(fp.out2_mod1)]
  )
}

# iv---generate replicates
replicate(reps,outFun_mod1())

# v---create data set of weighted (AICc) predictions
mod1.out <- fp.out.preds_mod1

colnames(mod1.out) <- make.names(
  colnames(mod1.out),
  unique = TRUE
)

mod1.out <- mod1.out |> 
  mutate(
    weight = as.numeric(
      avgMod$msTable[5] |> 
        head(1)
    )
  ) |> 
  relocate(
    weight,
    .after = V2
  ) |> 
  mutate(across(everything(), ~ .x * weight, .names = 'wt_{.col}')) |> 
  dplyr::select(
    -c(
      wt_V1,
      wt_V2,
      wt_weight
    )
  ) |> 
  dplyr::select(
    starts_with('V') | starts_with('wt')
  )

# b---second component model
# i---generate predictions from fixed effects
fixPredFun_mod2 <- function(sppNum,i){
  sarMeta.pred <- predict(
    predMod2,
    newmods = cbind(c(seq(2,20,1))),
    # transf = transf.ilogit,
    addx = TRUE
  ) |> 
    as.data.frame()
  # print(sarMeta.pred)
}

# ii---create output data frame to store replications
fp.out.preds_mod2 <- lapply(
  seq(0.5,3,0.5),
  function(i){
    cbind(c(seq(2,20,1)),c(rep(i,19)))
  }
) 

fp.out.preds_mod2 <-   do.call(
  rbind,
  fp.out.preds_mod2
) |> 
  as.data.frame()

# iii---apply function across a range of PITPH values
# *---generate predictions based on fixed effects
outFun_mod2 <- function(){
  fp.out_mod2 <- lapply(
    seq(0.5,3,0.5), 
    fixPredFun_mod2,
    sppNum = sppNum
  ) |> 
    bind_rows(.id = 'column_label')
  
  # *---add-in draws from simulated distributions (random-normal) of random effects
  fp.out2_mod2 <- fp.out_mod2 |> 
    mutate(
      fObs.id_mod2 = rnorm(
        nrow(fp.out_mod2),
        mean = paramObs_mod2$parameters[1],
        sd = paramObs_mod2$parameters[2]
      ),
      fMig.yr_mod2 = rnorm(
        nrow(fp.out_mod2), 
        mean = if_else(sppNum == 1, paramSTLmy_mod2$parameters[1], paramCHNmy_mod2$parameters[1]), 
        sd = if_else(sppNum == 1, paramSTLmy_mod2$parameters[2], paramCHNmy_mod2$parameters[2])
      ),
      fspp.code_mod2 = if_else(
        sppNum == 1, 
        as.numeric(
          randEffEsts_mod2$spp.code |>
            pull(intrcpt) |> 
            tail(1)
        ),
        as.numeric(
          randEffEsts_mod2$spp.code |>
            pull(intrcpt) |> 
            head(1)
        )
      ),
      fCSS.grp_mod2 = rnorm(
        nrow(fp.out_mod2), 
        mean = paramCSSgrp_mod2$parameters[1], 
        sd = paramCSSgrp_mod2$parameters[2]
      )
    ) |> 
    rowwise() |>
    mutate(
      logitPred_mod2 = rowSums(
        pick(
          pred, 
          fObs.id_mod2, 
          fMig.yr_mod2,
          fspp.code_mod2,
          fCSS.grp_mod2), 
        na.rm = FALSE
      )
    ) |>
    # *---back-transform predictions
    mutate(
      arithPred_mod2 = inv.logit(logitPred_mod2)
    ) |> 
    as.data.frame()
  
  # *---combine data into final (mean) predictions data frame  
  fp.out.preds_mod2 <<- cbind(
    fp.out.preds_mod2,
    fp.out2_mod2[,ncol(fp.out2_mod2)]
  )
}

# iv---generate replicates
replicate(reps,outFun_mod2())

# v---create data set of weighted (AICc) predictions
mod2.out <- fp.out.preds_mod2

colnames(mod2.out) <- make.names(
  colnames(mod2.out),
  unique = TRUE
)

mod2.out <- mod2.out |> 
  mutate(
    weight = as.numeric(
      avgMod$msTable[5] |> 
        tail(1)
    )
  ) |> 
  relocate(
    weight,
    .after = V2
  ) |> 
  mutate(across(everything(), ~ .x * weight, .names = 'wt_{.col}')) |> 
  dplyr::select(
    -c(
      wt_V1,
      wt_V2,
      wt_weight
    )
  ) |> 
  dplyr::select(
    starts_with('V') | starts_with('wt')
  )

# 3---combine output from component models into ensamble predictions
comb.mod <- bind_cols(
  mod1.out$V1,
  mod1.out$V2,
  mod1.out[,3:ncol(mod1.out)] + mod2.out[,3:ncol(mod2.out)]
)
# a---generate lower hdi for each wtt x pitph combination (rows)
lhdi.out <- lapply(
  seq(1.0,nrow(comb.mod),1.0),
  function(i){
    HDInterval::hdi(as.numeric(comb.mod[i,3:ncol(comb.mod)]))[1]
  }
) |> 
  bind_rows() |> 
  as.data.frame() |> 
  rename(
    lhdi = lower
  )

# b---generate upper hdi for each wtt x pitph combination (rows)
uhdi.out <- lapply(
  seq(1.0,nrow(comb.mod),1.0),
  function(i){
    HDInterval::hdi(as.numeric(comb.mod[i,3:ncol(comb.mod)]))[2]
  }
) |> 
  bind_rows() |>  
  as.data.frame() |>  
  rename(
    uhdi = upper
  )

# c---create summary output data frame
out.dat <- data.frame(
  wtt = comb.mod[,1],
  pitph = comb.mod[,2],
  mean = rowMeans(comb.mod[,3:ncol(comb.mod)]),
  median = rowMedians(as.matrix(comb.mod[,3:ncol(comb.mod)])),
  lhdi = pull(lhdi.out),
  uhdi = pull(uhdi.out)
)

# d---generate heat map
# i---read-in data
sarMeta_simsPlot.dat <- readRDS(
  paste0(
    'Output/Datasets/sarMeta_simsOut_',
    if_else(sppNum == 1,'stl','chn'),
    '.v.2025ar.rds'
  )
)

# ii---generate plot
sarMeta_HMp.plot <- ggplot(sarMeta_simsPlot.dat, aes(x=pitph, y=wtt, fill=mean)) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 14, margin = margin(t = 0, r = 5, b = 0, l = 0),family = 'Calibri'),
    axis.title.x = element_text(face = 'bold', size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0),family = 'Calibri'),
    axis.text.x = element_text(face = 'bold',size = 12,color='black', hjust=0.5,family = 'Calibri'),
    axis.text.y = element_text(face = 'bold',size = 12,color='black',family = 'Calibri'),
    legend.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
    plot.title = element_text(hjust = 0,size = 14,face = 'bold',family = 'Calibri')) +
  labs(title ='', y = 'WTT', x = 'PITPH') +
  geom_tile() +
  geom_text(aes(label=paste0(format(round(mean*100, 1), trim = TRUE), '%')), fontface = 2, size = 4.0, family = 'Calibri', color = 'black') +
  scale_fill_gradientn(colors = rainbow(if_else(sppNum == 1,12.0,3.0)), name = 'Mean SAR', breaks = c(0.01,0.02,0.03,0.04,0.05), labels = c('1.0%','2.0%','3.0%','4.0%','5.0%')) +
  scale_y_reverse(breaks = seq(2.0,20.0,1), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0.5,3.0,0.5), position = 'top', expand = c(0,0))

# iii--- print figure for review
print(sarMeta_HMp.plot)