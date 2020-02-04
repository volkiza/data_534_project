#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connexion")
}

#' @importFrom httr status_code
check_status <- function(res){
  stop_if_not(.x = status_code(res), 
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

base_url <- "https://api.lufthansa.com/v1/mds-references/countries/DK?limit=20&offset=0"
httr::GET(url = base_url, query = list(q = "Yeaye"))
