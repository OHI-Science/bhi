FP = function(layers, scores){

  # weights of FIS, MAR by rgn_id
  w <- SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
    dplyr::select(region_id = id_num,
                  w_FIS = val_num); head(w)

  # scores of FIS, MAR with appropriate weight
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages for potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  ## summarize scores as FP based on MAR, FIS weight
  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## return all scores
  return(rbind(scores, s))
} ## End FP Function
