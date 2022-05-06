#' Read bib
#'
#'
#' @title read_bib
#' @param file file
#' @param source one of the "CNKI","WanFang", and "VIP".
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @return A list.
#' @author Yuanlong Hu
#' @export

read_bib <- function(file, source="CNKI"){

  if(source %in% c("CNKI","WanFang","VIP")) stop("The source must be one of the `CNKI`, `WanFang`, and `VIP`")
  data <- read_lines(file = file, skip = 0, n_max = -1L)

  n <- list(
  RT = "^RT ",
  T1 = "^T1 ", A1 = "^A1 ",
  YR = "^YR ", JF = "^JF ",
  AB = "^AB ", K1 = "^K1 ",
  AD = "^AD ", SN = "^SN "
)

ck <- lapply(n, function(x){
  str_detect(data, x)
})

data <- data[ck$RT|ck$YR|ck$A1|ck$T1|ck$JF|ck$AB|ck$K1|ck$AD|ck$SN]

start <- which(str_detect(data, "^RT "))
end <- c(start[-1]-1, length(data))

s <- list()
for (i in 1:length(end)) s[[i]] <- c(start[i], end[i])

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
 data2$source <- source
 data2$ID <- paste0(data$JF, data$YR,"_", 1:nrow(data2))
 return(data2)
}
