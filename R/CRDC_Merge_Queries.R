#' Merge Query Results for both datasets available through the Civil Rights Documentation Collection (CRDC) APIs.
#'
#' This function allows users to merge the data available through the CRDC APIs. The function has the ability to preprocess any non-preproccesed query results.
#' Additionally, gives user the opportunity to only merge subsets of the query requests. An additional benefit of also querying results using standard query function by default.
#'
#' The function asks the user to define a set of function parameters (See arguements).
#' The Civil Rights Documentation Collection APIs contain two datasets.
#' The school enrollment 2013-14 dataset and the out-of-school suspensions 2013-14 datasets.
#' The datasets themselves are quite large, and during preliminary exploration users may only want a small subset of the data.
#' This function allows for large batch queries to be sent to the CRDC APIs.
#'
#' @param enrollmentdata This parameter is a dataframe that allows the user to define the enrollment data they want used for the merge. By default, the function runs a small batch query on the enrollment CRDC api.
#'
#' @param suspensiondata This parameter is a dataframe that allows the user to define the suspension data they want used for the merge. By default, the function runs a small batch query on the suspension CRDC api.
#'
#' @param preprocess This parameter is a logical which allows the user to turn on the optional preprocessing component of the function. By default this value is set to True.
#'
#' @param joinby This parameter is a string that allows the user to select which variables from the query he/she would to join the two datasets by. By defaul this parameter is set to "COMBOKEY".
#'
#' @param enrollvars This parameter is a character vector that allows the user to specify which of the enrollment dataset variables he/she would like returned. The Variable COMBOKEY is required as it is a used as the link between the two datasets. By Default this is sent to NULL.
#'
#' @param suspensionvars This parameter is a character vector that allows the user to specify which of the suspension dataset variables he/she would like returned. The Variable COMBOKEY is required as it is a used as the link between the two datasets. By Default this is sent to NULL.
#'
#'
#' @examples
#' CRDC_merged_data <- CRDC_Merge_Queries()
#'
#' CRDC_data <- CRDC_Merge_Queries(preprocess = FALSE, enrollvars = c("LEA_STATE","COMBOKEY","LEAID"))
#'
#' @importFrom magrittr %>%
#' @export


CRDC_Merge_Queries <- function(enrollmentdata = CRDC_Query(dataset = "enrollment", per_page = 100), suspensiondata = CRDC_Query(dataset = "suspension", per_page = 100), preprocess = T, joinby = "COMBOKEY", enrollvars = NULL, suspensionvars = NULL){


  if(is.null(enrollvars)){
    enrollmentdata <- enrollmentdata
  } else{
    if((joinby == "COMBOKEY" & !("COMBOKEY" %in% enrollvars)) | (joinby == "SCH_NAME" & !("SCH_NAME" %in% enrollvars))){
      stop("COMBOKEY or SCH_NAME must be included inorder to merge. Please add one of these to the list supplied to enrollvars parameter. It must match the joinby parameter")
    } else{
    enrollmentdata <- enrollmentdata %>%
      dplyr::select(enrollvars)
    }
  }

  if(is.null(suspensionvars)){
    suspensiondata <- suspensiondata
  } else{
    if((joinby == "COMBOKEY" & !("COMBOKEY" %in% suspensionvars)) | (joinby == "SCH_NAME" & !("SCH_NAME" %in% suspensionvars))){
      stop("COMBOKEY or SCH_NAME must be included inorder to merge. Please add one of these to the list supplied to suspensionvars parameter. It must match the joinby parameter")
    } else{
    suspensiondata <- suspensiondata %>%
      dplyr::select(suspensionvars)
    }
  }

  duplicate.colnames <- c("LEA_STATE", "LEA_NAME", "LEAID", "SCH_NAME")

  if(is.null(enrollvars)){
    if(is.null(suspensionvars)){
      duplicate.colnames <- duplicate.colnames
    } else{
      duplicate.colnames <- duplicate.colnames[(duplicate.colnames %in% suspensionvars)]
    }
  } else{
    if(is.null(suspensionvars)){
      duplicate.colnames <- duplicate.colnames[(duplicate.colnames %in% enrollvars)]
    } else{
      duplicate.colnames <- suspensionvars[(suspensionvars %in% enrollvars)]
    }
  }

  if(joinby %in% duplicate.colnames){
    duplicate.colnames <- duplicate.colnames[!(duplicate.colnames %in% joinby)]
  }

  if(length(duplicate.colnames) == length(suspensionvars)){
    stop("All Variables from the Suspension Dataset have been removed due to duplicative information from the Enrollment Dataset. Please choose a different set of variables for suspensionvars parameter")
  }

  CRDC_merged <- dplyr::left_join(enrollmentdata, dplyr::select(suspensiondata, -duplicate.colnames), by = joinby)

  if(preprocess == T){
    col.names <- c("LEA_NAME","LEA_STATE", "SCH_NAME","COMBOKEY","LEAID")

    suppressWarnings(CRDC_merged[!(colnames(CRDC_merged) %in% (col.names))] <- sapply(CRDC_merged[!(colnames(CRDC_merged) %in% (col.names))],as.numeric))

    CRDC_merged[CRDC_merged < 0] <- NA

  }

  return(CRDC_merged)

}

