#' @title Sorting the data
#'
#' @description Sorting the data for the disparity review by fuzzy matching
#'
#' @param data a column of the dataset
#' @param what a vector of categories
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

fuzzy.sort.data <- function(data, what) {
    ## Set all lower case for easy match
    what_l <- tolower(what)
    data_l <- tolower(data)

    ## Setting up the matching list
    total_matches <- data.frame()

    ## Loop through the fuzzy matching
    for(element in 1:length(what_l)) {
        ## Check fuzzy match
        fuzzy_match <- grep(what_l[element], data_l)
        length_match <- length(fuzzy_match)
        
        if(length_match > 0) {
            ## If match, record the matches
            matches <- data.frame("data" = fuzzy_match, "what" = rep(what[element], length_match))
            total_matches <- rbind(total_matches, matches)
        }
    }

    ## Getting the non-matching
    others <- which(!(1:length(data) %in% c(unique(total_matches$data))))
    length_others <- length(others)
    if(length_others > 0) {
        others <- data.frame("data" = others, "what" = rep("other", length_others))
        total_matches <- rbind(total_matches, others)
    }

    return(total_matches)
}