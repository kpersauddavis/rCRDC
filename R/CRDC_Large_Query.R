#' Large Query Request from Civil Rights Documentation Collection APIs.
#'
#' This function allows a user to make a query request from the CRDC APIs.
#' This is the large query function and returns any number of specified page results (or all pages) with per page results limited at 10,000.
#'
#' The function asks the user to define a set of function parameters (See arguements).
#' The Civil Rights Documentation Collection APIs contain two datasets.
#' The school enrollment 2013-14 dataset and the out-of-school suspensions 2013-14 datasets.
#' The datasets themselves are quite large, and during preliminary exploration users may only want a small subset of the data.
#' This function allows for large batch queries to be sent to the CRDC APIs.
#'
#' @param dataset This parameter is a string that can take one of two values to specify which dataset the user wants to query. The options are "enrollment" and "suspension". The parameter defaults to "enrollment".
#'
#' @param api_key This parameter a string representing the required api key to query the CRDC APIs. By default it is set to Sys.getenv("TOKEN"). Either replace with your user api key or specify in your Renviron file. API keys can be obtained at https://usedgov.github.io/key/.
#'
#' @param per_page This parameter is string representing the number of results per page you would like to see. This can be thought of as the number of observations that will be returned. By default it is set to 1000.
#'
#' @param sort This parameter is a string that allows the user to control how the output is sorted. By default this parameter is set to LEAID.
#'
#' @param page This parameter is a number representing which page of the results will be returned. By default this parameter is set to 1.
#'
#' @param preprocess This parameter is a logical which allows the user to turn on the optional preprocessing component of the function. By default this value is set to True.
#'
#' @param vars This parameter is a character vector that allows the user to select which variables from the query he/she would like returned. By defaul this parameter is set to NULL.
#'
#' @param numpages This parameter is a number that allows the user to specify how many pages of results the user would like returned. The default is set to NULL, therefore the query returns all pages.
#'
#' @examples
#' CRDC_data <- CRDC_Large_Query()
#'
#' CRDC_data <- CRDC_Large_Query(dataset = "suspension", per_page = "10000")
#'
#' CRDC_data <- CRDC_Large_Query(page = 2, vars = c("LEA_STATE","COMBOKEY","LEAID"))
#'
#' CRDC_data <- CRDC_Large_Query(dataset = "enrollment", numpages = 2, preprocess = FALSE)
#'
#' @importFrom magrittr %>%
#' @export


CRDC_Large_Query <- function(dataset = "enrollment", api_key = Sys.getenv("TOKEN"), per_page = "10000", sort = "LEAID", page = 1, numpages = NULL, preprocess = T, vars = NULL) {

  if(dataset == "enrollment"){
    endpoint = "https://api.ed.gov/data/crdc_enrollment_2013-14?"
  } else{
    endpoint = "https://api.ed.gov/data/crdc_oosuspensions_2013-14?"
  }



  query_params <- list("api_key" = api_key, "per_page" = per_page, "sort" = sort, "page" = page)

  CRDC_response <- httr::GET(endpoint, query = query_params)

  if(httr::http_error(CRDC_response)){
    stop("The request produced an error. Please check that api key and all other parameters are correct")
  } else{


    CRDC_data <- jsonlite::fromJSON(httr::content(CRDC_response, as = "text", encoding = "UTF-8"))

    CRDC_dataframe <- CRDC_data$resources

    if(is.null(numpages)){
      page_numbers <- CRDC_data$pagination$pages
    } else {
      page_numbers <- numpages
    }


    for(i in 2:page_numbers){
      query_params <- list("api_key" = api_key, "per_page" = per_page, "sort" = sort, "page" = i)
      CRDC_response <- httr::GET(endpoint, query = query_params)
      CRDC_data <- jsonlite::fromJSON(httr::content(CRDC_response, as = "text", encoding = "UTF-8"))
      CRDC_dataframe <- rbind(CRDC_dataframe, CRDC_data$resources)
      Sys.sleep(0.1)
    }

    if(preprocess == T){
      col.names <- c("LEA_NAME","LEA_STATE", "SCH_NAME","COMBOKEY","LEAID")

      suppressWarnings(CRDC_dataframe[!(colnames(CRDC_dataframe) %in% (col.names))] <- sapply(CRDC_dataframe[!(colnames(CRDC_dataframe) %in% (col.names))],as.numeric))

      CRDC_dataframe[CRDC_dataframe < 0] <- NA

    }

    if(is.null(vars)){
      return(CRDC_dataframe)
    } else{
      return(dplyr::select(CRDC_dataframe, vars))
    }

  }
}
