LSP = function(layers){

  # status

  lsp_status <- SelectLayersData(layers, layers = 'lsp_status') %>%
    dplyr::select(region_id = id_num,
                  score = val_num) %>%
    mutate(dimension = 'status'); head(lsp_status)

  # trend

  lsp_trend <-  SelectLayersData(layers, layers = 'lsp_trend') %>%
    dplyr::select(region_id = id_num,
                  score = val_num) %>%
    mutate(dimension = 'trend'); head(lsp_trend)

  # combine scores

  scores <- rbind(lsp_status, lsp_trend) %>%
    mutate(goal = 'LSP')

  return(scores)

} ## End LSP function
