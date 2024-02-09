# Function to calculate SMD

library(ggpubr)
SMD <- function(work3) {
  if (nrow(work3) > 0) {
    SMD_ML <- rma.uni(yi = work3$SMD,
                      vi = work3$SMDv,
                      method = "REML",
                      test = "t",
                      data = work3)
    return(SMD_ML)
  } else {
    return(NULL)  # Return NULL if there are no studies
  }
}

# get the file from df and make a new grouping
working <- df %>%
  mutate(GroupID = interaction(StudyId_I, ExperimentID_I, CohortId_I))

#get the n of cohorts with each outcome measure
group_characteristics <- working %>%
  group_by(GroupID, outcome_type) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = outcome_type, values_from = count, values_fill = 0) %>%
  ungroup()

group_characteristics$total <- rowSums(select(group_characteristics, -GroupID))
group_characteristics$GroupID <- as.character(group_characteristics$GroupID)

#put them in a grid
l <- ncol(group_characteristics) - 2

outcomes <- group_characteristics[, -c(1, 7)]
out_grid <- outcomes %>%
  mutate(across(everything(), ~ . > 0, .names = "{.col}_TF"))
out_grid <- out_grid[, -c(1:l)]

row_counts <- rowSums(out_grid, na.rm = TRUE)
result <- group_characteristics[row_counts >= 2, ]
result <- result[,c(1,4,3,5,2,6,7)]
colu <- as.data.frame(colnames(result))

for (j in 2:5) {
  for (k in (j+1):6) {
    outcome1 <- colu[j, 1]
    outcome2 <- colu[k, 1]
    
    if (!outcome1 == outcome2) {
      target <- subset(result, result[[outcome1]] > 0 & result[[outcome2]] > 0)
      
      if (nrow(target) > 0) {
        res <- data.frame()
        
        for (i in 1:nrow(target)) {
          work3 <- working %>% filter(GroupID %in% target[i, 1])
          work3 <- work3 %>% filter(outcome_type == outcome1)
          work3 <- work3 %>% filter(!is.na(SMDv))
          work3$SMD <- as.numeric(work3$SMD)
          work3$SMDv <- as.numeric(work3$SMDv)
          
          SMDx <- SMD(work3)
          
          res[i, 1] <- if (!is.null(SMDx)) SMDx$beta[[1]] else NA
          res[i, 2] <- if (!is.null(SMDx)) SMDx$se[[1]] else NA
          
          work4 <- working %>% filter(GroupID %in% target[i, 1])
          work4 <- work4 %>% filter(outcome_type == outcome2)
          work4 <- work4 %>% filter(!is.na(SMDv))
          work4$SMD <- as.numeric(work4$SMD)
          work4$SMDv <- as.numeric(work4$SMDv)
          
          SMDy <- SMD(work4)
          cn <- which(names(work4) == 'SortLabel')
          
          res[i, 3] <- if (!is.null(SMDy)) SMDy$beta[[1]] else NA
          res[i, 4] <- if (!is.null(SMDy)) SMDy$se[[1]] else NA
          res[i, 5] <- if (!is.null(SMDy)) SMDy$k[[1]] else NA
          res[i, 6] <- if (!is.null(SMDy)) work4[1,cn] else NA
        }
        
        labelx <- bquote(Delta ~ .(as.name(outcome1)))
        labely <- bquote(Delta ~ .(as.name(outcome2)))
        labelr <- paste0('correlation = ', sprintf("%.2f", summary(lm(res[, 3] ~ res[, 1], weights = 1/res[, 4]^2))$r.squared))
        # Your existing ggplot code
        # Your existing ggplot code
        p <- ggplot(data = res, aes(x = res[, 1], y = res[, 3], color = res[,6])) +
          geom_point(aes(size = res[, 5]), na.rm = TRUE) +  # Use res[, 5] as the size
          geom_errorbar(aes(ymin = res[, 3] - res[, 4], ymax = res[, 3] + res[, 4], width = 0.2), na.rm = TRUE) +
          geom_errorbar(aes(xmin = res[, 1] - res[, 2], xmax = res[, 1] + res[, 2], width = 0.2), na.rm = TRUE) +
          geom_smooth(method = "lm", se = TRUE, formula = y ~ x, 
                      aes(weight = 1/res[, 4]^2), color = "black", linetype = "dashed") +
          labs(x = labelx, y = labely, title = labelr) +
          coord_cartesian(clip = "off") +
          scale_size_continuous(name = 'Number of comparisons') +  
          scale_color_manual(name = 'Experiment type', 
                             values = c("CvS" = "red", "TvC" = "green"),
                             labels = c('Model', 'Drug')) +  # Set colors for SortLabel values
          expand_limits(y = 0) +
          expand_limits(x = 0)
        
ggsave(paste0("plot_", j, "_", k, ".png"), p, width = 8, height = 6)
      }
    }
  }
}
