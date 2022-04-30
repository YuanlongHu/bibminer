#' Read bib
#'
#'
#' @title read_bib
#' @param file file
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @return A list.
#' @author Yuanlong Hu
#' @export

read_bib <- function(file){

  data <- read_lines(file = file, skip = 0, n_max = -1L)

  start <- c(0, which(data == ""))+1
  end <- c(which(data == ""), length(data)+1)-1

  s <- list()
  for (i in 1:length(end)) s[[i]] <- c(start[i], end[i])
  n <- list(
   YR = "^YR ", A1 = "^A1 ",
   T1 = "^T1 ", JF = "^JF ",
   AB = "^AB ", K1 = "^K1 ",
   AD = "^AD ", SN = "^SN "
  )
  data2 <- lapply(s, function(x) {
    data <- data[x[1]:x[2]]
    res <- lapply(n, function(y) {
      d <- data[str_detect(data, y)]
      d <- ifelse(length(d)==0, NA, d)
      d <- gsub(y, "", d)
      }) %>% unlist()
    return(res)
    })
 data2 <- as.data.frame(Reduce(rbind,data2))
 return(data2)
}
