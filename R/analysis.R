library(doParallel)
library(parallel)
library(foreach)

sim_elast_of_sub <- function(scorescsv, calcyear = 2014, elast_of_sub = c(0, 1), niter = 1000, goal_weights = NULL){

  scorescsv <- filter(scorescsv, region_id %in% 1:42)
  goalscores <- filter(scorescsv, goal != "Index", dimension %in% c("status", "future"))
  if("year" %in% names(goalscores)){
    goalscores <- filter(goalscores, year == calcyear)
  }
  ## regions with one or more goal scores of zero will obtain an index value of zero
  ## to prevent this collapse to zero (looses some useful information), can set zero scores to small value
  ## no other scores have such small scores, so doesnt confuse signal (?)
  goalscores <- mutate(goalscores, score = ifelse(score == 0, 0.1, score))

  if(is.null(goal_weights)){
    goal_weights <- tibble::tibble(goal = unique(goalscores$goal), weight = rep(1, 18))
  }

  set.seed(10)
  nsample_uniform <- runif(niter, min = min(elast_of_sub), max = max(elast_of_sub))
  ## issues with calculations when sigma gets very small...
  ## remove any sigma values less than 0.0051
  nsample_uniform <- nsample_uniform[nsample_uniform >= 0.0051]
  nsample_uniform <- c(modelrun = nsample_uniform)

  cl <- makeCluster(3)
  registerDoParallel(cl)

  simulated_scores <- foreach(i = 1:length(nsample_uniform), .packages= c("tidyverse"), .combine = rbind) %dopar% {
    s <- nsample_uniform[[i]]

    simulated_scores <- goalscores %>%
      left_join(goal_weights, by = "goal") %>%
      mutate(weight = ifelse(is.na(score), NA, weight)) %>%
      group_by(dimension, region_id) %>%
      mutate(
        rgn_weight = weight/sum(weight, na.rm = TRUE),
        weightedscores = rgn_weight*score^((s-1)/s)
      ) %>%
      summarize(
        score = sum(weightedscores, na.rm = TRUE)^(s/(s-1))
      ) %>%
      group_by(region_id) %>%
      summarize(score = mean(score), dimension = "score") %>%
      arrange(desc(score)) %>%
      mutate(rank = rank(100-score)) %>%
      ungroup() %>%
      mutate(elasticity = paste("sigma", round(s, 7)), modelrun = names(nsample_uniform[i]))
  }
  stopCluster(cl)
  closeAllConnections()

  ## result with original (arithmetic) scores included
  scores_arithmetic <- scorescsv %>%
    filter(goal == "Index", dimension %in% c("score")) %>%
    arrange(desc(score)) %>%
    mutate(rank = rank(100-score)) %>%
    ungroup() %>%
    mutate(elasticity = "infinite", modelrun = "modelrun0") %>%
    select(-goal)

  ## quick plots to check results
  plotdf <- mutate(simulated_scores, region = as.factor(paste("region", region_id)))
  plotdf$region <- factor(plotdf$region, levels = paste("region", scores_arithmetic$region_id))
  scoreplot <- ggplot(plotdf) +
    geom_point(aes(region, score), size = 1.2, alpha = 0.01, color = "steelblue") +
    coord_flip() +
    labs(x = NULL, y =  NULL)
  rankplot <- ggplot(plotdf, aes(region, rank)) +
    geom_boxplot(outlier.size = 0.2, size = 0.2, alpha = 0.2, fill = "steelblue", color = "royalblue") +
    coord_flip() +
    labs(x = NULL, y =  NULL)

  result <- rbind(scores_arithmetic, simulated_scores)

  return(result)
}
