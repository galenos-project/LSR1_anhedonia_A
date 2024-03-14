filter_experiment_outcome_type <- function(df, experiment_type, outcome) {
  df_by_outcome <- df %>%
    filter(outcome_type == outcome) %>%
    filter(!is.na(SMD) | !is.na(SMDv))
  
  df_by_experiment_outcome <- df_by_outcome %>%
    filter(SortLabel == experiment_type)
  
  return(df_by_experiment_outcome)
}

create_formula <- function(factor_names, data) {
  distinct_levels <- sapply(factor_names, function(factor) n_distinct(data[[factor]]))
  
  if (all(distinct_levels < 5)) {
    # If all factors have fewer than 5 distinct levels, return NULL
    return(NULL)
  }
  
  selected_factors <- factor_names[distinct_levels >= 5]
  
  if (length(selected_factors) == 0) {
    # If none of the factors has enough levels, use a default grouping variable
    formula_str <- "~1"
  } else {
    formula_str <- paste("~ 1 |", paste(selected_factors, collapse = "/"))
  }
  
  formula_obj <- as.formula(formula_str)
  return(formula_obj)
}

  
run_ML_SMD <- function(df, experiment, outcome, rho_value) {
  df <- filter_experiment_outcome_type(df, experiment, outcome)
  df <- df %>% filter(!is.na(SMDv))
  
  # List of factors to consider
  factors_to_consider <- c("Strain", "StudyId", "ExperimentID_I")
  
  # Create the random effects formula
  random_formula <- create_formula(factors_to_consider, df)
  
  if (is.null(random_formula)) {
    cat("Insufficient levels for random effects grouping. Skipping meta-analysis.\n")
    return(NULL)
  }
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  
  # calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup = ExperimentID_I,
                    obs = effect_id,
                    data = df, 
                    rho = rho_value)
  
  # Use the formula in your rma.mv model
  SMD_ML <- rma.mv(yi = SMD,
                   V = VCVM_SMD,
                   random = random_formula,
                   test = "t",
                   data = df,
                   dfs = "contain",
                   control = list(optimizer = "nlm")
  )
  
  cat("Meta analysis summary:\n")
  print(summary(SMD_ML))
  
  cat("\n-------------------------\n")
  
  cat("Prediction Interval:\n")
  
  pred_interval <- predict(SMD_ML)
  
  print(pred_interval)
  
  return(list(SMD_ML = SMD_ML, pred_interval = pred_interval))
}


forest_metafor <- function(model, experiment_type, outcome_title) {
  if(!is.null(model)){
  
  
  lower_x <- floor(min(model$SMD_ML[["yi"]]- model$SMD_ML[["vi"]])) - 1
  lower_x <- floor(lower_x / 5) * 5
  upper_x <- ceiling(max(model$SMD_ML[["yi"]] + model$SMD_ML[["vi"]])) + 1
  upper_x <- ceiling(upper_x / 5) * 5
  arange <- upper_x - lower_x
  xleft <- -(1.5*arange)+lower_x
  xright <- upper_x + (0.5 * arange)
  summary_x <- model$SMD_ML[["beta"]]
  model$SMD_ML[["data"]][["SMD"]] <- round(model$SMD_ML[["data"]][["SMD"]],2)
  
  at_values <- seq(lower_x, upper_x, by = 2.5)
  
         forest_plot <- if(experiment_type == "TvC"){
                               forest(model$SMD_ML,
                                      xlim=c(xleft, xright),
                                      ylim=c(-2, model$SMD_ML$k+5), rows=c((model$SMD_ML$k+2):3),
                                      mlab="SMD [95% C.I.]", 
                                      alim=c((lower_x), (upper_x)),
                                      slab=paste(word(Authors_I, 1), Year, Strain),
                                      at = at_values,
                                      col = c("grey","grey"),
                                      addfit = TRUE,
                                      addpred = TRUE,
                                      annotate = TRUE,
                                      header = "Study and Strain",
                                      order=StudyId,
                                      xlab = "",
                                      ilab = cbind(ARRIVEScore, Label),
                                      ilab.xpos = c((lower_x-(0.72*arange)),(lower_x-(0.20*arange))),
                                      lty = c("solid","solid","solid"),
                                      cex = 0.75, 
                                      cex.axis = 1.0, 
                                      cex.lab = 1.2,
                                      efac = c(1,1,2))
           text(c((lower_x-(0.72*arange)),(lower_x-(0.20*arange))), model$SMD_ML$k+5, c("Reporting\n completeness", "Drug"), cex=0.75, font=2)
         } else {
                               forest(model$SMD_ML,
                                      xlim=c(xleft, xright),
                                      ylim=c(-2, model$SMD_ML$k+6), rows=c((model$SMD_ML$k+2):3),
                                      mlab="SMD [95% C.I.]",
                                      alim=c(lower_x, upper_x),
                                      slab=paste(word(Authors_I, 1), Year, Strain),
                                      at = at_values,
                                      col = c("grey","grey"),
                                      addfit = TRUE,
                                      addpred = TRUE,
                                      annotate = TRUE,
                                      header = "Study and Strain",
                                      order=StudyId,
                                      xlab = "", 
                                      ilab = cbind(ARRIVEScore, `DiseaseModelLabel(s)[1]_I`),
                                      ilab.xpos = c((lower_x-(0.52*arange)),(lower_x-(0.12*arange))),
                                      cex = 0.75, 
                                      cex.axis = 1.0, 
                                      cex.lab = 1.2,
                                      lty = c("solid","solid","solid"),
                                      efac = c(1,1,3))
           text(c((lower_x-(0.52*arange)),(lower_x-(0.12*arange))), model$SMD_ML$k+6, c("Reporting\n completeness", "Model"), cex=0.75, font=2)
         }
         
cixlower <- model$SMD_ML[["ci.lb"]]
cixhigher <- model$SMD_ML[["ci.ub"]]


  #mtext(outcome_title, side = 1, line = 3, cex = 1.2, font = 2)
  
  if (experiment_type == "TvC") {
    mtext("Favours control", side = 1, line = 3, at = 1.8*lower_x, cex = 1.1, col = "red", font = 1, adj = 0)
    mtext("Favours intervention", side = 1, line = 3, at = upper_x, cex = 1.1, col = "darkgreen", font = 1, adj = 1)
    #addpoly(model, row = 0.25, cex = 0.4, col = "darkred", mlab = "SMD", annotate = FALSE, xvals = c(cixlower, cixhigher))
    #mtext(paste0("SMD: ", round(model$beta, 2), " (", round(model$ci.lb, 2), " to ", round(model$ci.ub, 2), ")"), side = 3, line = -1, cex = 1, font = 2)
    title(paste0("Effect of dopaminergic drugs on ", outcome_title, " in models of depression (SMD)"))
    
  } else if (experiment_type == "CvS") {
    mtext("Model increases\nanhedonia", side = 1, line = 3, at = 1.5*lower_x, cex = 1.1, col = "red", font = 1, adj = 0)

    mtext("Model reduces\nanhedonia", side = 1, line = 3, at = (1.7*upper_x), cex = 1.1, col = "darkgreen", font = 1, adj = 1)

    #addpoly(model, row = 0.25, cex = 0.4, col = "darkred", mlab = "SMD", annotate = FALSE, xvals = c(cixlower, cixhigher))    
    #mtext(paste0("SMD: ", round(model$beta, 2), " (", round(model$ci.lb, 2), " to ", round(model$ci.ub, 2), ")"), side = 3, line = -1, cex = 1, font = 2)
    title(paste0("Effect of depression modelling on ", outcome_title, " (SMD)"))
    
  } 
}}

subgroup_analysis <- function(df, experiment_type, outcome, moderator, rho_value) {
  # this returns a table of effect sizes etc by moderator, for passing to 'forest_subgroup'
  # for plotting
  
  # Ensure the moderator is a character string for later conversion to symbol
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert character to factor if necessary
  if (is.character(df2[[moderator]])) {
    df2[[moderator]] <- factor(df2[[moderator]])}
  
  # List of factors to consider
  factors_to_consider <- c("Strain", "StudyId", "ExperimentID_I")
  
  # Create the random effects formula
  random_formula <- create_formula(factors_to_consider, df2)
  
  if (is.null(random_formula)) {
    cat("Insufficient levels for random effects grouping. Skipping meta-analysis.\n")
    return(NULL)
  }
  # Add a check for the number of levels in the moderator variable
  if (length(levels(df2[[moderator]])) >= 1) {
    #  message("In this iteration of the review, there was insufficient data to perform subgroup analysis for this variable (data for one subgroup only)")
    #  return(NULL)
    #}
    
    
    df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
    
    #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
    
    VCVM_SMD <- vcalc(vi = SMDv,
                      cluster = StudyId, 
                      subgroup= ExperimentID_I,
                      obs=effect_id,
                      data = df2, 
                      rho = rho_value) 
    
    # ML model on df2 with subgroup
    subgroup_analysis <- rma.mv(
      yi = SMD,
      V = VCVM_SMD,
      random = random_formula,
      data = df2,
      mods = as.formula(paste("~", moderator, "-1")),
      method = 'REML',
      test = "t",
      dfs = "contain"
    )
    
    #subgroup_analysis_predict <- predict(subgroup_analysis)
    
    ## ML model on df2 without subgroup
    overall_estimate_rma <- rma.mv(yi = SMD,
                                   V = VCVM_SMD,
                                   random = random_formula, # nested levels
                                   test = "t", # use t- and F-tests for making inferences
                                   data = df2,
                                   rho = rho_value,
                                   dfs="contain", # improve degree of freedom estimation for t- and F-distributions
                                   control=list(optimizer="nlminb"))
    
    #overall_estimate_rma_predict <- predict(overall_estimate_rma)
    
    
    k_subgroups <- df2 %>%
      group_by(df2[[moderator]]) %>%
      count() %>%
      pull(n)
    
    
    subgroup_analysis_plotdata <- data.frame(levels(df2[[moderator]]), k_subgroups, subgroup_analysis$beta, subgroup_analysis$se, subgroup_analysis$pval, subgroup_analysis$ci.lb, subgroup_analysis$ci.ub)
    colnames(subgroup_analysis_plotdata) <- c(moderator, "k", "SMD", "se","p", "ci_l", "ci_u") #, "pi.lb", "pi.ub")
    subgroup_analysis_plotdata$symbol <- 15
    subgroup_analysis_plotdata$size <- (1/subgroup_analysis_plotdata$se)
    subgroup_analysis_plotdata$summary <- FALSE
    subgroup_analysis_plotdata$fontfaace <- "plain"
    subgroup_analysis_plotdata$fontsize <- 3.88
    subgroup_analysis_plotdata=rbind(subgroup_analysis_plotdata, c("Overall estimate",  overall_estimate_rma$k, overall_estimate_rma$beta, overall_estimate_rma$se, overall_estimate_rma$pval, overall_estimate_rma$ci.lb,overall_estimate_rma$ci.ub, 18,1,TRUE,"bold",5)) #overall_estimate_rma_predict$pi.lb, overall_estimate_rma_predict$pi.ub))
    
    
    
    
    rownames(subgroup_analysis_plotdata) <- 1:nrow(subgroup_analysis_plotdata)
    subgroup_analysis_plotdata$k <- as.numeric(subgroup_analysis_plotdata$k)
    subgroup_analysis_plotdata$SMD <- as.numeric(subgroup_analysis_plotdata$SMD)
    subgroup_analysis_plotdata$se <- as.numeric(subgroup_analysis_plotdata$se)
    subgroup_analysis_plotdata$ci_l <- as.numeric(subgroup_analysis_plotdata$ci_l)
    subgroup_analysis_plotdata$ci_u <- as.numeric(subgroup_analysis_plotdata$ci_u)
    subgroup_analysis_plotdata$p <- as.numeric(subgroup_analysis_plotdata$p)
    subgroup_analysis_plotdata$symbol <- as.numeric(subgroup_analysis_plotdata$symbol)
    subgroup_analysis_plotdata$size <- as.numeric(subgroup_analysis_plotdata$size)
    subgroup_analysis_plotdata$fontsize <- as.numeric(subgroup_analysis_plotdata$fontsize)
    
    subgroup_analysis_plotdata$d1 <- (subgroup_analysis_plotdata$SMD - subgroup_analysis_plotdata$ci_l)/1.92
    subgroup_analysis_plotdata$d2 <- subgroup_analysis_plotdata$ci_u - subgroup_analysis_plotdata$SMD
    
    return(list(plotdata = subgroup_analysis_plotdata, 
                analysis = subgroup_analysis))
  }
}

plot_subgroup_analysis <- function(df, experiment_type, outcome, moderator, model_sub, model_main) {
  
  # Ensure the moderator is a character string for later conversion to symbol
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert character to factor if necessary
  if (is.character(df2[[moderator]])) {
    df2[[moderator]] <- factor(df2[[moderator]])}
  
  # Add a check for the number of levels in the moderator variable
  if (length(levels(df2[[moderator]])) <= 1) {
    message("In this iteration of the review, there was insufficient data to perform subgroup analysis for this variable (data for one subgroup only)")
    return(NULL)
  }
  
  if ((n_distinct(df$StudyId) > 2) & (n_distinct(df$ExperimentID_I) >10)) {
    #df2$RoBScore <- as.numeric(df2$RoBScore)
    #df2$RoBScore <- factor(df2$RoBScore, levels = c(0, 1, 2))
    
    
    #df2<-df2 %>% 
    #filter(SMD>-6) %>% 
    #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
    
    # df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
    # 
    # #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
    # 
    # VCVM_SMD <- vcalc(vi = SMDv,
    #                   cluster = StudyId, 
    #                   subgroup= ExperimentID_I,
    #                   obs=effect_id,
    #                   data = df2, 
    #                   rho = rho_value) 
    # 
    # # ML model on df2 with subgroup
    # subgroup_analysis <- rma.mv(
    #   yi = SMD,
    #   V = VCVM_SMD,
    #   random = ~1 | Strain / StudyId / ExperimentID_I,
    #   data = df2,
    #   mods = as.formula(paste("~", moderator, "-1")),
    #   method = 'REML',
    #   test = "t",
    #   dfs = "contain"
    # )
    # 
    # #subgroup_analysis_predict <- predict(subgroup_analysis)
    # 
    # ## ML model on df2 without subgroup
    # overall_estimate_rma <- rma.mv(yi = SMD,
    #                                V = VCVM_SMD,
    #                                random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
    #                                test = "t", # use t- and F-tests for making inferences
    #                                data = df2,
    #                                dfs="contain", # improve degree of freedom estimation for t- and F-distributions
    #                                control=list(optimizer="nlm"))
    # 
    # #overall_estimate_rma_predict <- predict(overall_estimate_rma)
    # 
    # k_subgroups <- df2 %>%
    #   group_by(df2[[moderator]]) %>%
    #   count() %>%
    #   pull(n)
    # 
    # 
    # subgroup_analysis_plotdata <- data.frame(levels(df2[[moderator]]), k_subgroups, subgroup_analysis$beta, subgroup_analysis$se) #subgroup_analysis_predict$pi.lb, subgroup_analysis_predict$pi.ub)
    # colnames(subgroup_analysis_plotdata) <- c(moderator, "k", "SMD", "se") #, "pi.lb", "pi.ub")
    # subgroup_analysis_plotdata=rbind(subgroup_analysis_plotdata, c("Overall estimate",  overall_estimate_rma$k, overall_estimate_rma$beta, overall_estimate_rma$se)) #overall_estimate_rma_predict$pi.lb, overall_estimate_rma_predict$pi.ub))
    # 
    # rownames(subgroup_analysis_plotdata) <- 1:nrow(subgroup_analysis_plotdata)
    # subgroup_analysis_plotdata$k <- as.numeric(subgroup_analysis_plotdata$k)
    # subgroup_analysis_plotdata$SMD <- as.numeric(subgroup_analysis_plotdata$SMD)
    # subgroup_analysis_plotdata$se <- as.numeric(subgroup_analysis_plotdata$se)
    # #subgroup_analysis_plotdata$ci_l <- as.numeric(subgroup_analysis_plotdata$ci_l)
    # #subgroup_analysis_plotdata$ci_u <- as.numeric(subgroup_analysis_plotdata$ci_u)
    
    overall_estimate_index <-dim(model_sub$plotdata)[1]
    
    if (moderator == "ARRIVEScoreCat") {
      sorted_data <- model_sub$plotdata[-overall_estimate_index, ]
      sorted_data <- sorted_data[order(sorted_data[[moderator]]), ]
    } else {
      sorted_data <- model_sub$plotdata[-overall_estimate_index, ]
    }
    
    options(digits=3)
    
    meta.all = metagen(TE = SMD, 
                       seTE = se, 
                       studlab = sorted_data[[moderator]], 
                       data = sorted_data, 
                       sm = "SMD", 
                       common = F)
    meta.all$TE.random <- model_sub$plotdata$SMD[overall_estimate_index]
    meta.all$seTE.random <- model_sub$plotdata$se[overall_estimate_index]
    meta.all$lower.random <- model_sub$plotdata$ci_l[overall_estimate_index]
    meta.all$upper.random <- model_sub$plotdata$ci_u[overall_estimate_index]
    meta.all$lower.predict <- model_main$pred_interval$pi.lb
    meta.all$upper.predict <- model_main$pred_interval$pi.ub
    
    
    if (moderator == "ARRIVEScoreCat") {
      
      # forest() call without sortvar
      forest(meta.all,
                  xlab="SMD",
                  smlab=outcome,
                  just="right",
                  addrow=F,
                  prediction = T,
                  overall=T,
                  overall.hetstat =F,
                  print.pval.Q=F,
                  col.square="black",
                  col.by="black",
                  fill.equi="aliceblue",
                  leftcols = c(moderator, "k"),
                  leftlabs = c(moderator, "Number of experiments")
      )
    } else {
      # forest() call with sortvar=seTE
      forest(meta.all,
                  xlab="SMD",
                  smlab=outcome,
                  just="right",
                  addrow=F,
                  overall=T,
                  prediction = T,
                  overall.hetstat =F,
                  print.pval.Q=F,
                  col.square="black",
                  sortvar=seTE,
                  col.by="black",
                  fill.equi="aliceblue",
                  leftcols = c(moderator, "k"),
                  leftlabs = c(moderator, "Number of experiments")
      )
    }
  }}


# forest_subgroup <- function(modelsumm, moderator, outcome, moderator_text, titletype) {
#    # this uses GGplot2 to draw a forest plot for the subgroup analyses, and returns the plot 
#   
#     if(titletype == "CvS"){
#       title <- paste0("Effect of modelling depression on ",outcome, " by ", moderator_text)  }
#     if(titletype == "TvC"){
#         title <- paste0("Effect of dopaminergic intervention on ",outcome, " by ", moderator_text)  }
#         
#     
#     model <- modelsumm
#     colnames(model) <- c('moderator','k','SMD','se','p','ci_l','ci_u','symbol','size','summary','fontfaace','fontsize','d1','d2')
#     model$order <- as.numeric(rownames(model))
#     model$estimate_lab = paste0(round(model$SMD,2), " (", round(model$ci_l,2), "; ", round(model$ci_u,2),")")
#     model <- model %>%
#       arrange(order) %>%
#       mutate(moderator = factor(model[["moderator"]], levels = unique(model[["moderator"]])))
#   lnth <- nrow(model)+1
#   
#   axis_min <- min(floor(min(model$ci_l, model$ci_u)),-2)
#   axis_max <- max(ceiling(max(model$ci_l, model$ci_u)),1)
#   span2 <- 1 + (axis_max - axis_min)
#   span1 <- span2 * 0.8
#   span3 <- span2 * 0.5
#   r1 <- span1
#   l2 <- span1 + 1
#   r2 <- span1 + span2 + 1
#   l3 <- span1 + span2 + 2
#   r3 <- span1 + span2 + span3 + 2
#   
#   cf <- span2/lnth
#   
# 
#   poly1 <- subset(model, model$moderator == "Overall estimate")
#   upp <- 1 + ((poly1$SMD - poly1$ci_l)/(cf *2))
#   lop<- 1 - ((poly1$SMD - poly1$ci_l)/(cf *2))
#   dfp <- data.frame(x = c(poly1$SMD, poly1$ci_u, poly1$SMD, poly1$ci_l), y = c(lop, 1, upp, 1))
# 
#   if(moderator == 'TreatmentDurationCategory'){
#       dord <- c('Overall estimate', 'More than 4 weeks', 'Between 1-4 weeks','Less than 1 week')
#         
#       p_mid <- model %>%
#       ggplot(aes(y = factor(moderator, levels = dord))) +
#       theme_classic() +
#       geom_point(aes(x = SMD), shape = model$symbol, size = model$size) +
#       geom_linerange(aes(xmin = ci_l, xmax = ci_u)) +
#       labs(x = "SMD Effect size") +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(axis_min-1, axis_max+1)) +
#       geom_vline(xintercept = 0, linetype = "solid") +
#       geom_vline(xintercept = poly1$SMD, linetype = "dashed") +
#       annotate("text", x = axis_min-1, y = lnth, label = "Favours\ncontrol", hjust = 0) +
#       annotate("text", x = axis_max+1, y = lnth, label = "Favours\nintervention", hjust = 1) +
#       geom_polygon(data = dfp, aes(x = x, y = y), fill = "grey") +
#       theme(axis.line.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.title.y = element_blank())
# 
#     p_left <-
#       model %>%
#       ggplot(aes(y = factor(moderator, levels = dord)))  +
#       geom_text(aes(x = 0, label = moderator), hjust = 0, size = model$fontsize) +
#       geom_text(aes(x = r1, label = k), hjust = 1, size = model$fontsize) +
#       theme_void() +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(0, span1))
#     
#     p_right <-
#       model %>%
#       ggplot() +
#       geom_text(aes(x = span3, y = factor(moderator, levels = dord), label = estimate_lab), size = model$fontsize, hjust = 1) +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(0, span3)) +
#       theme_void()
#     
#     layout <- c(
#       area(t = 0, l = 0, b = 30, r = r1),
#       area(t = 1, l = l2, b = 30, r = r2),
#       area(t = 0, l = l3, b = 30, r = r3)
#     )
#  
#     p_left + p_mid + p_right + plot_layout(design = layout) + plot_annotation(title = title, theme = theme(plot.title = element_text(hjust = 0.5)))
#   } else {
#     p_mid <- model %>%
#       ggplot(aes(y = fct_rev(moderator))) +
#       theme_classic() +
#       geom_point(aes(x = SMD), shape = model$symbol, size = model$size) +
#       geom_linerange(aes(xmin = ci_l, xmax = ci_u)) +
#       labs(x = "SMD Effect size") +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(axis_min-1, axis_max+1)) +
#       geom_vline(xintercept = 0, linetype = "solid") +
#       geom_vline(xintercept = poly1$SMD, linetype = "dashed") +
#       annotate("text", x = axis_min-1, y = lnth, label = "Favours\ncontrol", hjust = 0) +
#       annotate("text", x = axis_max+1, y = lnth, label = "Favours\nintervention", hjust = 1) +
#       geom_polygon(data = dfp, aes(x = x, y = y), fill = "grey") +
#       theme(axis.line.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.title.y = element_blank())
#     
#     p_left <-
#       model %>%
#       ggplot(aes(y = fct_rev(moderator))) +
#       geom_text(aes(x = 0, label = moderator), hjust = 0, size = model$fontsize) +
#       geom_text(aes(x = r1, label = k), hjust = 1, size = model$fontsize) +
#       theme_void() +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(0, span1))
#     
#     p_right <-
#       model %>%
#       ggplot() +
#       geom_text(aes(x = span3, y = fct_rev(moderator), label = estimate_lab),size = model$fontsize, hjust = 1) +
#       coord_cartesian(ylim = c(0, lnth), xlim = c(0, span3)) +
#       theme_void()
#     
#     layout <- c(
#       area(t = 0, l = 0, b = 30, r = r1),
#       area(t = 1, l = l2, b = 30, r = r2),
#       area(t = 0, l = l3, b = 30, r = r3)
#     )
#     
#     p_left + p_mid + p_right + plot_layout(design = layout) + plot_annotation(title = title, theme = theme(plot.title = element_text(hjust = 0.5)))
#     
#   }}



#overall_estimate_index <-dim(subgroup_analysis_plotdata)[1]
#  
#  if (moderator == "ARRIVEScoreCat") {
#    sorted_data <- subgroup_analysis_plotdata[-overall_estimate_index, ]
#    sorted_data <- sorted_data[order(sorted_data[[moderator]]), ]
#  } else {
#    sorted_data <- subgroup_analysis_plotdata[-overall_estimate_index, ]
#  }
#  
#  options(digits=3)
#  
#  meta.all = metagen(TE = sorted_data$SMD, 
#                     seTE = sorted_data$se, 
#                     studlab = sorted_data[[moderator]], 
#                     data = sorted_data, 
#                     sm = "SMD", 
#                     common = F)
#  meta.all$TE.random <- subgroup_analysis_plotdata$SMD[overall_estimate_index]
#  meta.all$seTE.random <- subgroup_analysis_plotdata$se[overall_estimate_index]
#  
#  
#
#  
#  if (moderator == "ARRIVEScoreCat") {
# 
#    
# forest() call without sortvar
#x <- forest(meta.all,
#                xlab="SMD",
#                smlab=outcome,
#                just="right",
#                addrow=F,
#                overall=T,
#                overall.hetstat =F,
#                print.pval.Q=F,
#                col.square="black",
#                col.by="black",
#                fill.equi="aliceblue",
#                leftcols = c(moderator, "k"),
#                leftlabs = c(moderator, "Number of experiments"),
#                fontsize = 8,
#                spacing = 0.5,
#                squaresize = 0.8
#    )
#  } else {
#    # forest() call with sortvar=seTE
#    x <- forest(meta.all,
#                xlab="SMD",
#                smlab=outcome,
#                just="right",
#                addrow=F,
#                overall=T,
#                overall.hetstat =F,
#                print.pval.Q=F,
#                col.square="black",
#                sortvar=seTE,
#                col.by="black",
#                fill.equi="aliceblue",
#                leftcols = c(moderator, "k"),
#                leftlabs = c(moderator, "Number of experiments"),
#                fontsize = 8,
#                spacing = 0.5,
#                squaresize = 0.8
#    )
#  }
#  
#  
#  return(list(
#    subgroup_analysis = subgroup_analysis,
#    subgroup_rma_summary = subgroup_analysis_plotdata,
#    x
#    ))
#}}

metaregression_analysis <- function(df, experiment_type, outcome, moderator, rho_value) {
  
  # Ensure the moderator is a character string for evaluation in sym() function (can't convert numerics to symbol)
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert moderator back to numeric
  df2[[moderator]] <- as.numeric(df2[[moderator]])
  
  #df2<-df2 %>% 
    #filter(SMD>-6) %>% 
    #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
  
  df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df2, 
                    rho = rho_value) 
  
  # Metaregression
  
  metaregression <- rma.mv(
    yi = SMD,
    V = VCVM_SMD,
    random = ~1 | Strain / StudyId / ExperimentID_I,
    data = df2,
    mods = as.formula(paste("~", moderator)),
    method = 'REML',
    test = "t",
    dfs = "contain"
  )
  
  metaregression_summary <- summary(metaregression)
  
  x <- bubble_plot(metaregression, 
              group = "StudyId",
              mod = moderator, 
              xlab = moderator, 
              ylab = "SMD", 
              legend.pos = "none", 
              k = TRUE) 
  
  return(list(
    metaregression = metaregression,
    metaregression_summary = metaregression_summary,
    regression_plot = x))
}

metaregression_analysis_by_drug <- function(df, experiment_type, outcome, drug_name, moderator, rho_value) {
  
  # Ensure the moderator is a character string for evaluation in sym() function (can't convert numerics to symbol)
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>% 
    filter(DrugName == drug_name) %>% #THIS IS ONLY CHANGE
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  if ((n_distinct(df2$StudyId) > 2) & (n_distinct(df2) >10)) {
    
  
  
  # Convert moderator back to numeric
  df2[[moderator]] <- as.numeric(df2[[moderator]])
  
  #df2<-df2 %>% 
  #filter(SMD>-6) %>% 
  #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
  
  df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df2, 
                    rho = rho_value) 
  
  # Metaregression
  
  metaregression <- rma.mv(
    yi = SMD,
    V = VCVM_SMD,
    random = ~1 | Strain / StudyId / ExperimentID_I,
    data = df2,
    mods = as.formula(paste("~", moderator)),
    method = 'REML',
    test = "t",
    dfs = "contain"
  )
  
  return(metaregression)
  }}
  
metaregression_plot_by_drug <- function(x, df, experiment_type, outcome, moderator, drug_name) {
  
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>% 
    filter(DrugName == drug_name) %>% #THIS IS ONLY CHANGE
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  if ((n_distinct(df2$StudyId) > 2) & (n_distinct(df2) >10)) {
  plot <- bubble_plot(x,
                   group = "StudyId",
                   mod = moderator, 
                   xlab = paste0("Dose of ", drug_name, " (mg/kg)"),
                   ylab = "SMD", 
                   legend.pos = "none", 
                   k = TRUE) 
  
  return(plot)
  }
}


###########################################
### risk of bias visualisation function ###
###########################################

SyRCLE_RoB_summary <- function(df, experiment_type, outcome) {
  
  df <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) 


RoB <- unique(df[,c(4,6,12,26:58)])

#change studyId to Author, year
RoB$StudyId <- toupper(paste0(str_extract(RoB$Authors_I,"\\b\\w+\\b"),', ',RoB$Year_I))

# fix >1 publication per first author in a year

# Assuming your data frame is named RoB and the column is named StudyId
unique_study_ids <- unique(RoB$StudyId)
suffix_list <- character(length = nrow(RoB))

for (study_id in unique_study_ids) {
  indices <- RoB$StudyId == study_id
  if (sum(indices) > 1) {
    suffix_list[indices] <- letters[seq_along(suffix_list[indices])]
  }
}

RoB$suffix <- suffix_list

# Add the suffix to the original column
RoB$StudyId <- paste(RoB$StudyId, RoB$suffix, sep = "")

# Remove the 'suffix' column if you no longer need it
RoB <- select(RoB, -suffix)
RoB <- RoB[order(RoB$StudyId),]


#extract Syrcle RoB scores
SyRCLE <- RoB[,c(37,4:13)]

#Change "yes' to 'low' and 'No' to 'high'
SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'Yes', 'Low', .)))
SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'No', 'High', .)))


colnames(SyRCLE) <- c('Study ID','Allocation sequence','Baseline similarity','Concealment of allocation sequence','Random housing','Caregivers blinded','Random selection for outcome assessment','Blinded outcome assessor','Incomplete data reporting addressed','Free from selective outcome reporting','Free of other risks of bias')
RoB_summary <- rob_summary(data <- SyRCLE, tool = "Generic", weighted = FALSE, overall = FALSE)

return(RoB_summary)
}

SyRCLE_RoB_traffic <- function(df, experiment_type, outcome) {
  
  df <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) 
  

  RoB <- unique(df[,c(4,6,12,26:58)])

  #change studyId to Author, year
  RoB$StudyId <- toupper(paste0(str_extract(RoB$Authors_I,"\\b\\w+\\b"),', ',RoB$Year_I))
  
  # fix >1 publication per first author in a year
  
  # Assuming your data frame is named RoB and the column is named StudyId
  unique_study_ids <- unique(RoB$StudyId)
  suffix_list <- character(length = nrow(RoB))
  
  for (study_id in unique_study_ids) {
    indices <- RoB$StudyId == study_id
    if (sum(indices) > 1) {
      suffix_list[indices] <- letters[seq_along(suffix_list[indices])]
    }
  }
  
  RoB$suffix <- suffix_list
  
  # Add the suffix to the original column
  RoB$StudyId <- paste(RoB$StudyId, RoB$suffix, sep = "")
  
  # Remove the 'suffix' column if you no longer need it
  RoB <- select(RoB, -suffix)
  RoB <- RoB[order(RoB$StudyId),]
  
  
  #extract Syrcle RoB scores
  SyRCLE <- RoB[,c(37,4:13)]
  
  #Change "yes' to 'low' and 'No' to 'high'
  SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'Yes', 'Low', .)))
  SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'No', 'High', .)))
  
  
  colnames(SyRCLE) <- c('Study','Allocation sequence','Baseline similarity','Concealment of allocation sequence','Random housing','Caregivers blinded','Random selection for outcome assessment','Blinded outcome assessor','Incomplete data reporting addressed','Free from selective outcome reporting','Free of other risks of bias')
  RoB_TL <- rob_traffic_light(data <- SyRCLE, tool = "Generic", psize = 6, overall = FALSE)
  
  return(RoB_TL)
}


ARRIVE_summary <- function(df, experiment_type, outcome) {
  
  df <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome)
  

  RoB <- unique(df[,c(4,6,12,26:58)])

  #change studyId to Author, year
  RoB$StudyId <- toupper(paste0(str_extract(RoB$Authors_I,"\\b\\w+\\b"),', ',RoB$Year_I))
  
  # fix >1 publication per first author in a year
  
  # Assuming your data frame is named RoB and the column is named StudyId
  unique_study_ids <- unique(RoB$StudyId)
  suffix_list <- character(length = nrow(RoB))
  
  for (study_id in unique_study_ids) {
    indices <- RoB$StudyId == study_id
    if (sum(indices) > 1) {
      suffix_list[indices] <- letters[seq_along(suffix_list[indices])]
    }
  }
  
  RoB$suffix <- suffix_list
  
  # Add the suffix to the original column
  RoB$StudyId <- paste(RoB$StudyId, RoB$suffix, sep = "")
  
  # Remove the 'suffix' column if you no longer need it
  RoB <- select(RoB, -suffix)
  RoB <- RoB[order(RoB$StudyId),]

  #extract ARRIVE reporting scores
ARRIVE <- RoB[,c(37,14:36)]

#Change "yes' to 'low' and 'No' to 'high'
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'Yes', 'Low', .)))
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'No', 'High', .)))
#but ethics NAs to justification into ethics low risk
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'NA (ethical approval declared)', 'Low', .)))
#combine desc stats and variance with ES and CI
ARRIVE <- ARRIVE %>%
  mutate(Data_reporting = ifelse(ARRIVE$`(ARRIVE) Are desc stats for each exp group provided with measure of variability?_I` == 'Low' | ARRIVE$`(ARRIVE) Is the effect size and confidence interval provided?_I` == 'Low', 'Low', 'High'))
ARRIVE <- ARRIVE[,c(1:17,25,20:24)]


colnames(ARRIVE) <- c('Study','Groups clearly defined','Experimental unit defined','Exact number of experimental units','Sample size justification',
                      'Inclusion and exclusion criteria given','Any exclusions reported','Randomisation for any experiments','Blinding to group allocation',
                      'Details of what was measured','Statistical approach for each outcome','Assessment of whether data met statistical assumptions',
                      'All species specified','Animal sex specified','Age, weight or developmental stage specified','Timing and frequency of proceedures described',
                      'Any acclimitisation described','Data with variance, or Effect size and CI','Ethical approval with approval number',
                      'Ethical approval with or without approval number','Conflicts of interest statement','Funding sources','Description of any role of funder')
Rep_summary <- rob_summary(data <- ARRIVE, tool = "Generic", weighted = FALSE, overall = FALSE)

return(Rep_summary)
}

ARRIVE_traffic <- function(df, experiment_type, outcome) {
  
  dfa <- subset(df, df$SortLabel == experiment_type)
  dfb <- subset(dfa, dfa$outcome_type == outcome)


  RoB <- unique(dfb[,c(4,6,12,26:58)])

  #change studyId to Author, year
  RoB$StudyId <- toupper(paste0(str_extract(RoB$Authors_I,"\\b\\w+\\b"),', ',RoB$Year_I))
  
  # fix >1 publication per first author in a year
  
  # Assuming your data frame is named RoB and the column is named StudyId
  unique_study_ids <- unique(RoB$StudyId)
  suffix_list <- character(length = nrow(RoB))
  
  for (study_id in unique_study_ids) {
    indices <- RoB$StudyId == study_id
    if (sum(indices) > 1) {
      suffix_list[indices] <- letters[seq_along(suffix_list[indices])]
    }
  }
  
  RoB$suffix <- suffix_list
  
  # Add the suffix to the original column
  RoB$StudyId <- paste(RoB$StudyId, RoB$suffix, sep = "")
  
  # Remove the 'suffix' column if you no longer need it
  RoB <- select(RoB, -suffix)
  RoB <- RoB[order(RoB$StudyId),]
  
  #extract ARRIVE reporting scores
  ARRIVE <- RoB[,c(37,14:36)]
  
  #Change "yes' to 'low' and 'No' to 'high'
  ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'Yes', 'Low', .)))
  ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'No', 'High', .)))
  #but ethics NAs to justification into ethics low risk
  ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'NA (ethical approval declared)', 'Low', .)))
  #combine desc stats and variance with ES and CI
  ARRIVE <- ARRIVE %>%
    mutate(Data_reporting = ifelse(ARRIVE$`(ARRIVE) Are desc stats for each exp group provided with measure of variability?_I` == 'Low' | ARRIVE$`(ARRIVE) Is the effect size and confidence interval provided?_I` == 'Low', 'Low', 'High'))
  ARRIVE <- ARRIVE[,c(1:17,25,20:24)]
  
  
  colnames(ARRIVE) <- c('Study','Groups clearly defined','Experimental unit defined','Exact number of experimental units','Sample size justification',
                        'Inclusion and exclusion criteria given','Any exclusions reported','Randomisation for any experiments','Blinding to group allocation',
                        'Details of what was measured','Statistical approach for each outcome','Assessment of whether data met statistical assumptions',
                        'All species specified','Animal sex specified','Age, weight or developmental stage specified','Timing and frequency of proceedures described',
                        'Any acclimitisation described','Data with variance, or Effect size and CI','Ethical approval with approval number',
                        'Ethical approval with or without approval number','Conflicts of interest statement','Funding sources','Description of any role of funder')
  Rep_TL <- rob_traffic_light(data = ARRIVE, tool = "Generic", psize = 6, overall = FALSE)
  
  return(Rep_TL)
}

run_sse_NMD <- function(df, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(NMDv)) %>%
    filter(outcome_type == "Locomotor activity") %>%
    filter(SortLabel == "TvC")
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$NMDSE <- sqrt(df$NMDv)
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_NMD <- vcalc(vi = NMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  NMD_sse <- rma.mv(yi = NMD,
                    V = VCVM_NMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ NMDSE, # sampling error (squart root of sampling variance SMDV);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  return(NMD_sse)
}

run_sse_plot <- function(df, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(NMDv)) %>%
    filter(outcome_type == "Locomotor activity") %>%
    filter(SortLabel == "TvC")
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$NMDSE <- sqrt(df$NMDv)
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_NMD <- vcalc(vi = NMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  NMD_sse <- rma.mv(yi = NMD,
                    V = VCVM_NMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ NMDSE, # sampling error (squart root of sampling variance SMDV);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  plot <- bubble_plot(NMD_sse, mod = "NMDSE", group = "StudyId", xlab = "SE of NMD estimate", ylab = "NMD estimate")
  return(plot)
}

############################################################
run_ML_NMD <- function(df, experiment, outcome, rho_value) {
  
  df<-filter_experiment_outcome_type(df, experiment, outcome) 
  
  df<-df %>% 
    filter(!is.na(NMDv))
    #filter(NMD>-600) %>% 
    #filter(NMD<600) # delete missing values and some weirdly large values, like -15 and 16
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  VCVM_NMD <- vcalc(vi = NMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value) 
  
  NMD_ML <- rma.mv(yi = NMD,
                   V = VCVM_NMD,
                   random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                   test = "t", # use t- and F-tests for making inferences
                   data = df,
                   dfs="contain",
                   control=list(optimizer="nlm")
  )
  
  #if (length(unique(df$StudyId)) > 1) {
  #NMD_ML <- robust(NMD_ML, cluster = StudyId, clubSandwich = FALSE)
  #}
  
  cat("Meta analysis summary:\n")
  print(summary(NMD_ML))
  
  cat("\n-------------------------\n")
  
  cat("Prediction Interval:\n")
  
  pred_interval <- predict(NMD_ML)
  
  print(pred_interval)
  
  return(NMD_ML)
}

forest_metafor_NMD <- function(model, outcome){
  
  lower_x <- floor(min(model[["yi"]]- model[["vi"]])) - 1
  lower_x <- floor(lower_x / 100) * 100
  upper_x <- ceiling(max(model[["yi"]] + model[["vi"]])) + 1
  upper_x <- ceiling(upper_x / 100) * 100
  arange <- upper_x - lower_x
  xleft <- -(1.5*arange)+lower_x
  xright <- upper_x + (0.5 * arange)
  summary_x <- model[["beta"]]
  model[["data"]][["NMD"]] <- round(model[["data"]][["NMD"]],1)
  
  at_values <- seq(lower_x, upper_x, by = 20)
  
  pred_interval <- predict(model)
  
         forest_plot <- forest(model, 
                               xlim=c(xleft, xright),
                               ylim=c(-2, model$k+5), rows=c((model$k+2):3),
                               mlab="NMD [95% CI]",
                               alim=c(lower_x-30, upper_x+20),
                               slab=paste(word(Authors_I, 1), Year, Strain),
                               at = seq(-500,500,100),
                               col = c("darkgrey","darkgrey"),
                               addfit = TRUE,
                               addpred = TRUE,
                               annotate = TRUE,
                               header = "Study and Strain",
                               order=StudyId,
                               xlab = "",
                               ilab = cbind(ARRIVEScore, Label),
                               ilab.xpos = c((lower_x-(0.72*arange)),(lower_x-(0.20*arange))),
                               cex = 0.75, 
                               cex.axis = 1.0, 
                               cex.lab = 1,
                               efac = c(1,1,2))
         text(c((lower_x-(0.72*arange)),(lower_x-(0.20*arange))), model$k+5, c("Reporting\n completeness", "Drug"), cex=0.75, font=2)

  
  #mtext(outcome, side = 1, line = 3, cex = 1.2, font = 2)
  mtext("Favours\n control", side = 1, line = 3, at = lower_x, cex = 1, col = "red", font = 1)
  mtext("Favours dopaminergic\n agent", side = 1, line = 3, at = (1.2*upper_x), cex = 1, col = "darkgreen", font = 1)
  title(paste0("Dopaminergic agents effect on ", outcome, " in psychosis (NMD)"))
  
}


###### Function to check number of levels in moderator variables for an experiment type (SortLabel) and outcome for this iteration #######

check_moderator_levels <- function(df, experiment, outcome) {
  
  moderator_vars <- c("Sex", "CategoryDiseaseInduction", "InterventionAdministrationRoute", 
                      "ProphylacticOrTherapeutic", "TreatmentDurationCategory", 
                      "DrugName", "Efficacy", "Selectivity")

  
  df2 <- filter_experiment_outcome_type(df, experiment, outcome)
  single_level_mods <- character()  # Initialize an empty character vector to store moderators
  
  for (moderator in moderator_vars) {
    # convert to factor if not already
    if (!is.factor(df2[[moderator]])) {
      df2[[moderator]] <- factor(df2[[moderator]])
    }
    
    # check number of levels
    moderator_levels <- df2 %>%
      group_by(!!sym(moderator)) %>%
      summarise(n = n_distinct(StudyId)) %>%
      ungroup() %>%
      summarise(moderator_levels = n_distinct(!!sym(moderator))) %>%
      pull(moderator_levels)
    
    if (moderator_levels < 2) {
      single_level_mods <- c(single_level_mods, moderator)
    }
  }
  
  # rename moderator variables in list to match names in the inline text
  
  single_level_mods1 <- single_level_mods %>% 
    str_replace("TreatmentDurationCategory", "Duration of treatment period") %>% 
    str_replace("InterventionAdministrationRoute", "Route of intervention administration") %>%
    str_replace("ProphylacticOrTherapeutic", "Prophylactic or therapeutic intervention") %>%
    str_replace("CategoryDiseaseInduction", "Disease induction method") %>%
    str_replace("DrugName", "Intervention admnistered (drug)") %>% 
    tolower()
  
  if (length(single_level_mods1) > 1) {
    x <- paste(head(single_level_mods1, -1), collapse = ", ")
    x <- paste(x, "and", tail(single_level_mods1, 1))
  } else {
    x <- single_level_mods1
  }
  
  return(x)  # Return the formatted string
}

check_moderator_levels_i <- function(df, experiment, outcome) {
  
  moderator_vars <- c("Sex", "CatDisInd", "InterventionAdministrationRoute", 
                      "ProphylacticOrTherapeutic", "TreatmentDurationCategory", 
                      "DrugName")

  
  df2 <- filter_experiment_outcome_type(df, experiment, outcome)
  single_level_mods <- character()  # Initialize an empty character vector to store moderators
  
  for (moderator in moderator_vars) {
    # convert to factor if not already
    if (!is.factor(df2[[moderator]])) {
      df2[[moderator]] <- factor(df2[[moderator]])
    }
    
    # check number of levels
    moderator_levels <- df2 %>%
      group_by(!!sym(moderator)) %>%
      summarise(n = n_distinct(StudyId)) %>%
      ungroup() %>%
      summarise(moderator_levels = n_distinct(!!sym(moderator))) %>%
      pull(moderator_levels)
    
    if (moderator_levels < 2) {
      single_level_mods <- c(single_level_mods, moderator)
    }
  }
  
  # rename moderator variables in list to match names in the inline text
  
  single_level_mods1 <- single_level_mods %>% 
    str_replace("TreatmentDurationCategory", "Duration of treatment period") %>% 
    str_replace("InterventionAdministrationRoute", "Route of intervention administration") %>%
    str_replace("ProphylacticOrTherapeutic", "Prophylactic or therapeutic intervention") %>%
    str_replace("CategoryDiseaseInduction", "Disease induction method") %>%
    str_replace("DrugName", "Intervention admnistered (drug)") %>% 
    tolower()
  
  if (length(single_level_mods1) > 1) {
    x <- paste(head(single_level_mods1, -1), collapse = ", ")
    x <- paste(x, "and", tail(single_level_mods1, 1))
  } else {
    x <- single_level_mods1
  }
  
  return(x)  # Return the formatted string
}

check_moderator_levels_m <- function(df, experiment, outcome) {
  
  moderator_vars <- c("Sex", "CatDisInd")
  
  
  df2 <- filter_experiment_outcome_type(df, experiment, outcome)
  single_level_mods <- character()  # Initialize an empty character vector to store moderators
  
  for (moderator in moderator_vars) {
    # convert to factor if not already
    if (!is.factor(df2[[moderator]])) {
      df2[[moderator]] <- factor(df2[[moderator]])
    }
    
    # check number of levels
    moderator_levels <- df2 %>%
      group_by(!!sym(moderator)) %>%
      summarise(n = n_distinct(StudyId)) %>%
      ungroup() %>%
      summarise(moderator_levels = n_distinct(!!sym(moderator))) %>%
      pull(moderator_levels)
    
    if (moderator_levels < 2) {
      single_level_mods <- c(single_level_mods, moderator)
    }
  }
  
  # rename moderator variables in list to match names in the inline text
  
  single_level_mods1 <- single_level_mods %>% 
    str_replace("TreatmentDurationCategory", "Duration of treatment period") %>% 
    str_replace("InterventionAdministrationRoute", "Route of intervention administration") %>%
    str_replace("ProphylacticOrTherapeutic", "Prophylactic or therapeutic intervention") %>%
    str_replace("CategoryDiseaseInduction", "Disease induction method") %>%
    str_replace("DrugName", "Intervention admnistered (drug)") %>% 
    tolower()
  
  if (length(single_level_mods1) > 1) {
    x <- paste(head(single_level_mods1, -1), collapse = ", ")
    x <- paste(x, "and", tail(single_level_mods1, 1))
  } else {
    x <- single_level_mods1
  }
  
  return(x)  # Return the formatted string
}
run_sse_SMD_L <- function(df, type, outcome, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(SMDv)) %>%
    filter(outcome_type == outcome) %>%
    filter(SortLabel == type)
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$SMDN <- 1/sqrt(as.numeric(df$NumberOfAnimals_C) + as.numeric(df$NumberOfAnimals_I))
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  SMD_sse <- rma.mv(yi = SMD,
                    V = VCVM_SMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ SMDN, # sampling error (squart root of N);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  return(SMD_sse)
}

run_sse_SMD_C <- function(df, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(SMDv)) %>%
    filter(outcome_type == "Cognition") %>%
    filter(SortLabel == "TvC")
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$SMDN <- 1/sqrt(as.numeric(df$NumberOfAnimals_C) + as.numeric(df$NumberOfAnimals_I))
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  SMD_sse <- rma.mv(yi = SMD,
                    V = VCVM_SMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ SMDN, # sampling error (squart root of N);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  return(SMD_sse)
}

run_sse_plot_SMD_L <- function(df, type, outcome, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(SMDv)) %>%
    filter(outcome_type == outcome) %>%
    filter(SortLabel == type)
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$SMDN <- 1/sqrt(as.numeric(df$NumberOfAnimals_C) + as.numeric(df$NumberOfAnimals_I))
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  SMD_sse <- rma.mv(yi = SMD,
                    V = VCVM_SMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ SMDN, # sampling error (squart root of N);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  plot <- bubble_plot(SMD_sse, mod = "SMDN", group = "StudyId", xlab = "1/SQRT(N)", ylab = "SMD estimate", legend.pos = "none")
  return(plot)
}

run_sse_plot_SMD_C <- function(df, rho_value = 0.5) {
  
  #  df<-filter_experiment_outcome_type(df, experiment, outcome)
  
  df<-df %>% 
    filter(!is.na(SMDv)) %>%
    filter(outcome_type == "Cognition") %>%
    filter(SortLabel == "TvC")
  
  df <- df %>% mutate(effect_id = row_number()) # add effect_id column
  df$SMDN <- 1/sqrt(as.numeric(df$NumberOfAnimals_C) + as.numeric(df$NumberOfAnimals_I))
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df, 
                    rho = rho_value)
  
  SMD_sse <- rma.mv(yi = SMD,
                    V = VCVM_SMD,
                    random = ~1 | Strain / StudyId / ExperimentID_I, # nested levels
                    mods = ~ SMDN, # sampling error (squart root of N);
                    test = "t", # use t- and F-tests for making inferences
                    data = df,
                    dfs="contain",
                    control=list(optimizer="nlm")
  )
  
  plot <- bubble_plot(SMD_sse, mod = "SMDN", group = "StudyId", xlab = "1/SQRT(N)", ylab = "SMD estimate", legend.pos = "none")
  return(plot)
}

subgroup_SMD <- function(df, experiment_type, outcome, moderator, rho_value) {
  # with intercept, to allow calculation of effect of moderators - returns intercept 
  # as beta-coefficient for first category, and beta coefficients for other categories 
  # compared with the intercept. We do not use for plotting or tabulating ES and CIs,
  # but do use it to calculate whether the efefct of moderators is significant
  
  # Ensure the moderator is a character string for later conversion to symbol
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert character to factor if necessary
  if (is.character(df2[[moderator]])) {
    df2[[moderator]] <- factor(df2[[moderator]])}
  
  # Add a check for the number of levels in the moderator variable
  if (length(levels(df2[[moderator]])) <= 1) {
    message("In this iteration of the review, there was insufficient data to perform subgroup analysis for this variable (data for one subgroup only)")
    return(NULL)
  }
  
  if ((n_distinct(df$StudyId) > 2) & (n_distinct(df$ExperimentID_I) >10)) {
    #df2$RoBScore <- as.numeric(df2$RoBScore)
    #df2$RoBScore <- factor(df2$RoBScore, levels = c(0, 1, 2))
    
    
    #df2<-df2 %>% 
    #filter(SMD>-6) %>% 
    #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
    
    df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
    
    #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
    
    VCVM_SMD <- vcalc(vi = SMDv,
                      cluster = StudyId, 
                      subgroup= ExperimentID_I,
                      obs=effect_id,
                      data = df2, 
                      rho = rho_value) 
    
    # ML model on df2 with subgroup
    subgroup_analysis <- rma.mv(
      yi = SMD,
      V = VCVM_SMD,
      random = ~1 | Strain / StudyId / ExperimentID_I,
      data = df2,
      mods = as.formula(paste("~", moderator)), #"-1")),
      method = 'REML',
      test = "t",
      dfs = "contain"
    )
    return(subgroup_analysis)
  }  }

subgroup_SMDI <- function(df, experiment_type, outcome, moderator, rho_value) {
  # this gives beta co-efficients for every moderator variable compared with no effect; 
  # so is used to report these and their 95% CIs, but not whether or not the effects of 
  # moderators is significant - for which we use subgroup_SMD, which includes an 
  # intercept in the model
  # Ensure the moderator is a character string for later conversion to symbol
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert character to factor if necessary
  if (is.character(df2[[moderator]])) {
    df2[[moderator]] <- factor(df2[[moderator]])}
  
  # Add a check for the number of levels in the moderator variable
  if (length(levels(df2[[moderator]])) <= 1) {
    message("In this iteration of the review, there was insufficient data to perform subgroup analysis for this variable (data for one subgroup only)")
    return(NULL)
  }
  
  if ((n_distinct(df$StudyId) > 2) & (n_distinct(df$ExperimentID_I) >10)) {
    #df2$RoBScore <- as.numeric(df2$RoBScore)
    #df2$RoBScore <- factor(df2$RoBScore, levels = c(0, 1, 2))
    
    
    #df2<-df2 %>% 
    #filter(SMD>-6) %>% 
    #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
    
    df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
    
    #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
    
    VCVM_SMD <- vcalc(vi = SMDv,
                      cluster = StudyId, 
                      subgroup= ExperimentID_I,
                      obs=effect_id,
                      data = df2, 
                      rho = rho_value) 
    
    # ML model on df2 with subgroup
    subgroup_analysis <- rma.mv(
      yi = SMD,
      V = VCVM_SMD,
      random = ~1 | Strain / StudyId / ExperimentID_I,
      data = df2,
      mods = as.formula(paste("~", moderator, "-1")),
      method = 'REML',
      test = "t",
      dfs = "contain"
    )
    return(subgroup_analysis)
  }  }

metaregression_analysisI <- function(df, experiment_type, outcome, moderator, rho_value) {
  
  # Ensure the moderator is a character string for evaluation in sym() function (can't convert numerics to symbol)
  moderator <- as.character(moderator)
  
  df2 <- df %>% 
    filter(SortLabel == experiment_type) %>% 
    filter(outcome_type == outcome) %>%  
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym(moderator))) # Filter out NA values in moderator column
  
  # Convert moderator back to numeric
  df2[[moderator]] <- as.numeric(df2[[moderator]])
  
  #df2<-df2 %>% 
  #filter(SMD>-6) %>% 
  #filter(SMD<6) # delete missing values and some weirdly large values, like -15 and 16
  
  df2 <- df2 %>% mutate(effect_id = row_number()) # add effect_id column
  
  #calculate variance-covariance matrix of the sampling errors for dependent effect sizes
  
  VCVM_SMD <- vcalc(vi = SMDv,
                    cluster = StudyId, 
                    subgroup= ExperimentID_I,
                    obs=effect_id,
                    data = df2, 
                    rho = rho_value) 
  
  # Metaregression
  
  metaregression <- rma.mv(
    yi = SMD,
    V = VCVM_SMD,
    random = ~1 | Strain / StudyId / ExperimentID_I,
    data = df2,
    mods = as.formula(paste("~", moderator, "-1")),
    method = 'REML',
    test = "t",
    dfs = "contain"
  )
  
  metaregression_summary <- summary(metaregression)
  
  x <- bubble_plot(metaregression, 
                   group = "StudyId",
                   mod = moderator, 
                   xlab = moderator, 
                   ylab = "SMD", 
                   legend.pos = "none", 
                   k = TRUE) 
  
  return(list(
    metaregression = metaregression,
    metaregression_summary = metaregression_summary,
    regression_plot = x))
}

### straight MA: SMD, REML

run_SMD <- function(df, experiment, outcome) {
  df <- filter_experiment_outcome_type(df, experiment, outcome)
  df <- df %>% filter(!is.na(SMDv))
  df$SMD <- as.numeric(df$SMD)
  df$SMDv <- as.numeric(df$SMDv)
  
  SMD_ML <- rma.uni(yi = SMD,
                    vi = SMDv,
                    method = "REML",
                    test = "t",
                    data = df)
  
  cat("Meta analysis summary:\n")
  print(summary(SMD_ML))
  
  cat("\n-------------------------\n")
  
  cat("Prediction Interval:\n")
  
  pred_interval <- predict(SMD_ML)
  
  print(pred_interval)
  
  return(SMD_ML)
}

drug_summary <- function(drug, outcome){
  diag1 <- df %>%
    filter(SortLabel == "TvC") %>%
    filter(outcome_type == outcome) %>%
    filter(DrugName == drug) %>%
    filter(!is.na(SMDv)) %>%
    filter(!is.na(!!sym("DoseOfIntervention_mgkg")))
  diag2 <- n_distinct(diag1$StudyId)
  diag3 <- nrow(diag1)
  
  output_text0 <- paste0(drug, ": There were ", diag3, " comparisons from ", diag2, " publication(s).")
  return(output_text0)
}
