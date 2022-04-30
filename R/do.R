#' do_dictionary
#'
#'
#' @title do_dictionary
#' @param data path
#' @importFrom dplyr %>%
#' @importFrom cidian decode_scel
#' @return A list.
#' @author Yuanlong Hu
#' @export

do_dictionary <- function(path, add_words =NULL){

  dir.path <- as.character(list.files(path = path, pattern = ".scel"))
  for (i in 1:length(dir.path)){
    decode_scel(scel = dir.path[i],output=paste(dir.path[i],".dict"),cpp = FALSE,progress =TRUE)
  }

  # merge
  txtfile <- list.files(pattern = ".dict")

  txtall <- c()
  for (i in 1:length(txtfile)) {
    temp <- read.table(txtfile[i], sep="\n", fileEncoding="utf-8", encoding ="utf-8")
    txtall <- rbind(txtall, temp)
  }

  if(!is.null(add_words)){
    add_words <- data.frame(V1 = paste(add_words, 1))
    txtall <- rbind(add_words, txtall)
  }
  txtall <- dplyr::distinct(txtall)

  write.table(txtall,file = "user.utf8.dict",quote = F,row.names = F,col.names = F,fileEncoding = "UTF-8")

}
