

#-------------------------------------------------------------------------------
#' Print human-readable messages for http errors.
#'
#' @param status_code The http response code.
#' @return Nothing.
check_response <- function(status_code) {
  if (status_code == 200) {
  } else if (status_code == 401) {
    stop('The provided key could not be authenticated (code=401).')
  } else if (status_code == 400) {
    stop('The server says your request was bad. Please check param value and
         other arguments (code=400).')
  } else if (status_code > 401 & status_code < 500) {
    stop(paste('Something bad happened. The problem is client-related (code=',
               status_code, ')', sep=''))
  } else if (status_code >= 500 & status_code < 600) {
    stop(paste('Something bad happened. The problem is server-related (code=',
               status_code, ')', sep=''))
  } else {
    stop(paste('Something bad happened (code=', status_code, ')', sep=''))
  }
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' Get all values a parameter can take.
#'
#' Get all values of a parameters that can be passed in a GET request. Primarily
#' used as a utility function by other functions.
#' See \url{https://quickstats.nass.usda.gov/api} for a table of parameter
#' names.
#'
#' @param key Your NASS api key.
#' @param param The parameter name to get values of.
#' @param short_desc The short_desc for which to get possible values of the
#' param.
#' @param source_desc The source_desc for which to get possible values of the
#' param.
#' @param year The year for which to get possible values of param.
#' @param agg_level_desc The agg_level_desc for which to get possible values of
#' param.
#' @return A vector of all values that the parameter can take.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_param_values(key=key, param='short_desc')
#' get_param_values(key=key, param='year',
#'                  short_desc='CORN, GRAIN - ACRES HARVESTED',
#'                  source_desc='CENSUS')
#' }
#' @export
get_param_values <- function(key,
                             param,
                             short_desc=NA,
                             source_desc=NA,
                             year=NA,
                             agg_level_desc=NA) {

  short_desc = gsub('&', '%26', short_desc)
  short_desc = gsub(' ', '+', short_desc)

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  url <- paste('http://quickstats.nass.usda.gov/api/get_param_values/?',
               'key=', key,
               '&param=', param,
               sep='')

  if (!is.na(short_desc)) {
    url <- paste(url, '&short_desc=', short_desc, sep='')
  }
  if (!is.na(source_desc)) {
    url <- paste(url, '&source_desc=', source_desc, sep='')
  }
  if (!is.na(year)) {
    url <- paste(url, '&year=', year, sep='')
  }
  if (!is.na(agg_level_desc)) {
    url <- paste(url, '&agg_level_desc=', agg_level_desc, sep='')
  }
  r <- httr::GET(url)
  check_response(r$status)
  items <- httr::content(r)
  results <- c()
  for (i in 1:length(items[[1]])) {
    results <- c(results, items[[1]][[i]][[1]])
  }
  return(results)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' Get the parameter options available for some short_desc value.
#'
#' Not all combinations of parameters are available for all data items. This
#' functions finds the unique combinations that are available.
#'
#' @param key Your NASS API key.
#' @param data_item The short_desc (data item) string to get options for.
#' @return A tibble df of the unique combinations of other paramters that are
#' available.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_options(key=key, data_item='CORN, GRAIN - ACRES HARVESTED')
#' }
#' @export
get_options <- function(key, data_item) {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  # sorry about the nesting!
  message('Retrieving options...this may take a minute...')
  combos <- list()
  possible_sources <- get_param_values(key=key,
                                       param='source_desc',
                                       short_desc=data_item)
  for (source_desc in possible_sources) {
    possible_years <- get_param_values(key=key,
                                       param='year',
                                       short_desc=data_item,
                                       source_desc=source_desc)
    for (year in possible_years) {
      if (year >= '1997') {
        possible_agg_level_desc <- get_param_values(key=key,
                                                    param='agg_level_desc',
                                                    short_desc=data_item,
                                                    source_desc=source_desc,
                                                    year=year)
        for (agg_level_desc in possible_agg_level_desc) {
          if (agg_level_desc == 'COUNTY' | agg_level_desc == 'STATE') {
            possible_domains <- get_param_values(
              key=key,
              param='domain_desc',
              short_desc=data_item,
              source_desc=source_desc,
              agg_level_desc=agg_level_desc,
              year=year)
            for (domain in possible_domains) {
              combos[[length(combos)+1]] <- c(source_desc,
                                              year,
                                              agg_level_desc,
                                              domain)
            }
          }
        }
      }
    }
  }
  mat <- do.call(rbind, combos)
  if (is.null(mat)) {
    message('The data item is not available at the state or county level.
             There are no options.')
    return(NULL)
  }
  colnames(mat) <- c('source_desc', 'year', 'agg_level_desc', 'domain_desc')
  df <- tibble::as_tibble(mat)
  return(df)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' Get available data items based on search terms.
#'
#' There are large number of data items available. This function can be used
#' to increasingly refine search results until the desired data item is found.
#'
#' @param key Your NASS api key.
#' @param search_terms A vector of search terms. Each result will include all
#' terms.
#' @param exclude A vector of search terms to exclude. No result will have any
#' of these.
#' @return A list of all search results.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' search_data_items(key, search_terms=c('corn', 'harvested'),
#'                   exclude=c('sweet'))
#' search_data_items(key, search_terms=c('corn', 'price'), exclude=c())
#' }
#' @export
search_data_items <- function(key, search_terms, exclude=c()) {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  items <- get_param_values(key, param='short_desc')
  results <- c()
  for (i in 1:length(items)) {
    # check for any exclude terms
    skip <- FALSE
    for (term in exclude) {
      if (grepl(toupper(term), items[i], fixed=TRUE)) {
        skip <- TRUE
        break
      }
    }
    # if any exclude terms were found, skip rest of loop
    if (skip) {
      next
    }
    # check for all search terms
    matches <- 0
    for (term in search_terms) {
      if (grepl(toupper(term), items[i], fixed=TRUE)) {
        matches <- matches + 1
      }
    }
    # if all search terms were present, add data item to results
    if (matches == length(search_terms)) {
      results <- c(results, items[i])
    }
  }

  return(results)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' Get the count of values that exist for the specified query for county-level
#' data.
#'
#' This is used as a utility function by other functions, but can also be used
#' to explore expected results before pulling real data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use
#' search_data_items function to find one.
#' @param fips Must be 'all', a 2-digit state fips, or a 5-digit county fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size
#' categories of operations), use 'all' to get all.
#' @param source Must be 'CENSUS' or 'SURVEY'.
#' @return The count of values.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_county_item_count(key=key, year=2017,
#'                       data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_county_item_count(key=key, year=2017,
#'                       data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' get_county_item_count(key=key, year=2017,
#'                       data_item='CORN, GRAIN - ACRES HARVESTED',
#'                       fips='08069', domain='all')
#' }
#' @export
get_county_item_count <- function(key, year, data_item, fips='all',
                                  domain='TOTAL', source='CENSUS') {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  domain = gsub('&', '%26', domain)
  domain = gsub(' ', '+', domain)

  data_item = gsub('&', '%26', data_item)
  data_item = gsub(' ', '+', data_item)

  base_url <- paste('http://quickstats.nass.usda.gov/api/get_counts/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year=', year,
                    '&agg_level_desc=COUNTY',
                    '&source_desc=', source,
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  # get a specific county
  else if (nchar(fips) == 5) {
    state_fips <- substr(fips, 1, 2)
    county_fips <- substr(fips, 3, 5)
    url <- paste(base_url, '&state_fips_code=', state_fips,
                 '&county_ansi=', county_fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips or a 5-digit
          county fips')
    return(NULL)
  }
  # make the request
  r <- httr::GET(url)
  check_response(r$status)
  return(httr::content(r)$count)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' A flexible function for pulling county-level data.
#'
#' Automatically builds the specified query and retrieves county-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use
#' search_data_items function to find one.
#' @param fips Must be 'all', a 2-digit state fips, or a 5-digit county fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size
#' categories of operations), use 'all' to get all.
#' @param source Must be 'CENSUS' or 'SURVEY'.
#' @return A tibble df of the requested data, if any exists. Otherwise returns
#' NULL.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_county_data(key=key, year=2017,
#'                 data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_county_data(key=key, year=2017,
#'                 data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' get_county_data(key=key, year=2017,
#'                 data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069',
#'                 domain='all')
#' }
#' @export
get_county_data <- function(key, year, data_item, fips='all',
                            domain='TOTAL', source='CENSUS') {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  domain = gsub('&', '%26', domain)
  domain = gsub(' ', '+', domain)

  data_item = gsub('&', '%26', data_item)
  data_item = gsub(' ', '+', data_item)

  # check if any data exists
  if (get_county_item_count(key, year, data_item, fips, domain, source) == 0) {
    print('No data exists for this particular query.
          Try modifying query paramters.')
    return(NULL)
  }

  base_url <- paste('http://quickstats.nass.usda.gov/api/api_GET/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year=', year,
                    '&agg_level_desc=COUNTY',
                    '&source_desc=', source,
                    '&format=CSV',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  # get a specific county
  else if (nchar(fips) == 5) {
    state_fips <- substr(fips, 1, 2)
    county_fips <- substr(fips, 3, 5)
    url <- paste(base_url, '&state_fips_code=', state_fips,
                 '&county_ansi=', county_fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips or a 5-digit
          county fips')
    return(NULL)
  }
  # make the request
  r <- httr::GET(url)
  check_response(r$status)
  return(httr::content(r))
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' Get the count of values that exist for the specified query for state-level
#' data.
#'
#' This is used as a utility function by other functions, but can also be used
#' to explore expected results before pulling real data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use
#' search_data_items function to find one.
#' @param fips Must be 'all' or a 2-digit state fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size
#' categories of operations), use 'all' to get all.
#' @param source Must be 'CENSUS' or 'SURVEY'.
#' @return The count of values.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_state_item_count(key=key, year=2017,
#'                      data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_state_item_count(key=key, year=2017,
#'                      data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' }
#' @export
get_state_item_count <- function(key, year, data_item, fips='all',
                                 domain='TOTAL', source='CENSUS') {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  domain = gsub('&', '%26', domain)
  domain = gsub(' ', '+', domain)

  data_item = gsub('&', '%26', data_item)
  data_item = gsub(' ', '+', data_item)

  base_url <- paste('http://quickstats.nass.usda.gov/api/get_counts/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year=', year,
                    '&agg_level_desc=STATE',
                    '&source_desc=', source,
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  else {
    print('The fips argument must be "all" or a 2-digit state fips')
    return(NULL)
  }
  # make the request
  r <- httr::GET(url)
  check_response(r$status)
  return(httr::content(r)$count)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' A flexible function for pulling state-level data.
#'
#' Automatically builds the specified query and retrieves state-level data.
#'
#' @param key Your NASS API key.
#' @param year Must be a census year (e.g. 2012, 2017).
#' @param data_item The long description of the desired series. Use
#' search_data_items function to find one.
#' @param fips Must be 'all' or a 2-digit state fips.
#' @param domain A modifier on data_item, some characterstic (e.g. size
#' categories of operations), use 'all' to get all.
#' @param source Must be 'CENSUS' or 'SURVEY'.
#' @return A tibble df of the requested data, if any exists. Otherwise returns
#' NULL.
#' @examples
#' \donttest{
#' key <- Sys.getenv('NASS_KEY')
#' get_state_data(key=key, year=2017,
#'                data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
#' get_state_data(key=key, year=2017,
#'                data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
#' }
#' @export
get_state_data <- function(key, year, data_item, fips='all',
                           domain='TOTAL', source='CENSUS') {

  if (!curl::has_internet()) {
    stop('No internet connection!')
  }

  domain = gsub('&', '%26', domain)
  domain = gsub(' ', '+', domain)

  data_item = gsub('&', '%26', data_item)
  data_item = gsub(' ', '+', data_item)

  # check if any data exists
  if (get_state_item_count(key, year, data_item, fips, domain, source) == 0) {
    print('No data exists for this particular query.
           Try modifying query paramters.')
    return(NULL)
  }

  base_url <- paste('http://quickstats.nass.usda.gov/api/api_GET/?',
                    'key=', key,
                    '&short_desc=', data_item,
                    '&year=', year,
                    '&agg_level_desc=STATE',
                    '&source_desc=', source,
                    '&format=CSV',
                    sep='')

  # add specific domain if needed
  if (domain != 'all') {
    base_url <- paste(base_url, '&domain_desc=', domain, sep='')
  }

  # get all counties in US
  if (fips == 'all') {
    url <- base_url
  }
  # get all counties in a state
  else if (nchar(fips) == 2) {
    url <- paste(base_url, '&state_fips_code=', fips, sep='')
  }
  else {
    stop('The fips argument must be "all" or a 2-digit state fips')
  }
  # make the request
  r <- httr::GET(url)
  check_response(r$status)
  return(httr::content(r))
}
#-------------------------------------------------------------------------------


