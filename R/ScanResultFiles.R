#' This function scans the result folder for missing files.
#'
#' @param ResultFolder path to the folder containing the results (simulations)
#' @param ModelList list of model structure to take into account
#'
#' @return prints on the screen two lists: 1) MID of existing files, 2) MID of missing files
#'
#' @export
#' 
#' @examples
#' # ScanResultFiles("/test", ModelList)
#'

ScanResultFiles <- function(ResultFolder,ModelList){

  # initialization
  StartFilename="MID_"
  existing_files <- missing_files <- array(NA,0)
  existing_counter <- missing_counter <- 0

  for (mid in ModelList[,"mid"]){

    if (file.exists(paste(ResultFolder,"/",StartFilename,mid,".Rdata",sep=""))) {
      existing_counter <- existing_counter + 1
      existing_files <- c(existing_files,mid)
    }else{
      missing_counter <- missing_counter + 1
      missing_files <- c(missing_files,mid)
    }

  }

  message(paste(length(existing_files),"existing files in the folder:"))
  print(existing_files)

  message(paste(length(missing_files),"missing files in the folder:"))
  print(missing_files)

}
