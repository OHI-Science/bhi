
CW = function(scores){
  #####----------------------######
  ## CW status & CW Trend

  ## UPDATE 15June 2016 - Jennifer Griffiths - update CW status and trend calculation for CW
  ## only have 1 status point for CON, TRA (therefore can not take a CW status as geometric mean with many points over time)
  ## Calculate geometric mean of 1 status for current status
  ## calculate arithmetic mean of NUT and CON trend for the CW trend (because have 0, NA, and neg. values in trend, cannot use geometric mean)


  ## Status is the geometric mean of NUT, CON, TRA status for most recent year
  ## trend in the arithmetic mean of NUT, CON, TRA trend because can not deal with 0 values in geometric mean

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  ## Function to deal with cases where want to take the arithmetic mean of a vector of NA values, will return NA instead of NaN
  mean_NAall = function(x){

    if(sum(is.na(x))==length(x)){mean_val = NA
    }else{mean_val =mean(x,na.rm=TRUE) }
    return(mean_val)
  }

  ## subset CW subgoals
  scores_cw <- scores %>%
    filter(goal %in% c('NUT', 'TRA', 'CON')) %>%
    arrange(dimension,region_id)

  ## Calculate geometric mean for status, arithmetic mean for trend (ignore NAs)
  ## NOTE to @jennifergriffiths: there are still several 'NaN's, perhaps because of TRA?
  ## 7 July 2016 - Think have fixed the NaN problem with the function mean_NAall()
  ## also, rounding score doesn't seem to work here; ends up with .00 precision. maybe round later?

  ## July 21, 2016 @jules32 after discussing with @Melsteroni.
  ## Calculate CW as a geometric mean from NUT, CON, and TRA
  ## for only 3 dimensions: status, likely future state and score.
  ## Calculate trend as a simple mean.
  s <- rbind(
    scores_cw %>%
      filter(dimension %in% c('status', 'future', 'score')) %>%
      group_by(region_id, dimension) %>%
      summarize(score = round(geometric.mean2(score, na.rm=TRUE))) %>% # round status to 0 decimals
      ungroup(),
    scores_cw %>%
      filter(dimension %in% c('trend')) %>%
      group_by(region_id, dimension) %>%
      summarize(score = mean(score, na.rm=TRUE)) %>%
      ungroup()) %>%
    arrange(region_id) %>%
    mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## return all scores
  return(rbind(scores, s))
} ## End CW function
