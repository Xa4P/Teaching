# Function to estimate utility based on the EQ-5D-5L
estimate_utility <- function(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1, tbl_decrements = NL_tariff){
  ##mo = mobility level (1 to 5)
  ##sc = self-care level (1 to 5)
  ##ua = usual activities level (1 to 5)
  ##pd = pain/ discomfort level (1 to 5)
  ##ad = anxiety/ discomfort level (1 to 5)
  ##tbl_decrements = table with decrements, with 2 columns (Levels and Decrements), order by category and levels
  
  res <- numeric() # object to store result
  v_levels <- c(mo, sc, ua, pd, ad) # create vector of levels
  v_possible_levels <- c(1:5)
 
  try(if(all(v_levels %in% v_possible_levels) == FALSE) stop("levels different than 1 to 5"))

  if(sum(v_levels) == 5) {
    res <- 1
  } else{
    
    constant <- tbl_decrements[which(tbl_decrements$Levels == "Constant"), "Decrements"] # Constant when deviating from full health
    utility <- constant + 
      ifelse(v_levels[1] == 1, 0, tbl_decrements[which(tbl_decrements$Levels == "mo2") + (v_levels[1] - 2), "Decrements"]) +
      ifelse(v_levels[2] == 1, 0, tbl_decrements[which(tbl_decrements$Levels == "sc2") + (v_levels[2] - 2), "Decrements"]) +
      ifelse(v_levels[3] == 1, 0, tbl_decrements[which(tbl_decrements$Levels == "ua2") + (v_levels[3] - 2), "Decrements"]) +
      ifelse(v_levels[4] == 1, 0, tbl_decrements[which(tbl_decrements$Levels == "pd2") + (v_levels[4] - 2), "Decrements"]) +
      ifelse(v_levels[5] == 1, 0, tbl_decrements[which(tbl_decrements$Levels == "ad2") + (v_levels[5] - 2), "Decrements"]) # apply decrements
    
    res <- utility
    }
  
  return(res)
}

