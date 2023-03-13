#' score_npss
#'
#' @param input_df Dataframe containing the 29 NPSS items, with the prefix: 'npss'
#' @param missing_threshold A percentage of allowed missing data for each subscale in the NPSS (Default .5). If missingness exceeds the set threshold, function returns NA for subscale score.
#'
#' @return Input dataframe with NPSS scores added.
#' @export
#'
#' @examples
#' score_npss(test_data, missing_threshold = .4)
score_npss <- function(input_df, missing_threshold = .5){
  # Add unique identifier for merging
  input_df <- dplyr::mutate(input_df, unique_id_for_merging = dplyr::row_number())


  # converts NPSS_prefix to lowercase if needed
  if(any(grepl('NPSS', names(input_df)))){
    df <- dplyr::rename_at(input_df, dplyr::vars(dplyr::starts_with('NPSS')),
                    ~tolower(.))
    upper_prefix = T
  } else {
    df <- input_df
    upper_prefix = F
  }


  npss_count <- sum(grepl('^npss', names(df)))

  if(!npss_count > 1){
    stop("Could not find any NPSS variables. Check your data to make sure the prefix npss is present on all columns with Safety Scale Data")
  }
  if(npss_count != 29){
    stop('There are not 29 NPSS Variables. Please check your data to make sure there are exactly 29 variables with the prefix \'npss\'')
  }



  if(suppressWarnings(all(lapply(dplyr::select(df, dplyr::starts_with('npss')), is.numeric)))){
    npss_range <- range(dplyr::select(df, dplyr::starts_with('npss')), na.rm = T)

    if(!(npss_range[1] >= 1 & npss_range[2] <= 5)){
      stop('Range of NPSS is not between 1 and 5, please check your data')
    }


    df <- dplyr::mutate_at(df, dplyr::vars(dplyr::starts_with('npss')),
                    ~dplyr::case_when(. == 1 ~ 'Strongly Disagree' ,
                                      . == 2 ~ 'Disagree',
                                      . == 3 ~ 'Neither Agree Nor Disagree',
                                      . == 4 ~ 'Agree',
                                      . == 5 ~ 'Strongly Agree'))
    numeric_tf = T

  }else{
    numeric_tf = F
  }




  # Isolate and factor variables
  npss_raw <- dplyr::mutate_at(df, dplyr::vars(dplyr::starts_with('npss')),
                               ~stringr::str_to_title(.))

  npss_raw <- dplyr::mutate_at(npss_raw, dplyr::vars(dplyr::starts_with('npss')),
                        ~factor(., levels = c("Strongly Disagree",
                                                     "Disagree",
                                                     "Neither Agree Nor Disagree",
                                                     "Agree",
                                                     "Strongly Agree")))

  s <- dplyr::select(npss_raw, dplyr::starts_with('npss'))

  if(!all(!grepl("\\s+[a-z]", s))){ # Test whether first character in each word is capitalized (Title Case Check)
    warning('Strings are not in Title Case, please check that each word starts with a capital in your data')
  }



  # Convert missing threshold to decimal
  if (missing_threshold > 1){
    missing_threshold = missing_threshold / 100
  }


  # Convert variables to numeric
  npss_num <- dplyr::mutate_at(npss_raw, dplyr::vars(dplyr::starts_with('npss')),
                        ~dplyr::case_when(. == 'Strongly Disagree' ~ 1,
                                   . == 'Disagree' ~ 2,
                                   . == 'Neither Agree Nor Disagree' ~ 3,
                                   . == 'Agree' ~ 4,
                                   . == 'Strongly Agree' ~ 5))

  npss_num <-
    # add suffix to numeric items
    dplyr::rename_at(npss_num, dplyr::vars(dplyr::matches('npss(.*)\\d$')),
              ~paste0(., '_num'))

  ## Total Scale Sums and Means

  npss_num <-
    dplyr::mutate(npss_num,
           npss_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::matches('npss(.*)_num'))))  # checks if missing values exceed missing threshold for each row
                             < ncol(dplyr::select(npss_num, dplyr::matches('npss(.*)_num'))) * missing_threshold,
                             rowSums(dplyr::select(npss_num, dplyr::matches('npss(.*)_num')), na.rm = T),
                             NA),
           npss_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::matches('npss(.*)_num'))))  # checks if missing values exceed missing threshold for each row
                              < ncol(dplyr::select(npss_num, dplyr::matches('npss(.*)_num'))) * missing_threshold,
                              rowMeans(dplyr::select(npss_num, dplyr::matches('npss(.*)_num')), na.rm = T),
                              NA))



    # Calculate Subscales
    npss_num <-
    dplyr::mutate(npss_num,
           npss_soc_engage_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_1_num:npss_14_num)))
                                        < ncol(dplyr::select(npss_num, npss_1_num:npss_14_num)) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, npss_1_num:npss_14_num), na.rm = T), NA),
           npss_compassion_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_15_num:npss_21_num)))
                                        < ncol(dplyr::select(npss_num, npss_15_num:npss_21_num)) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, npss_15_num:npss_21_num), na.rm = T), NA),
           npss_body_sense_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_22_num:npss_29_num)))
                                        < ncol(dplyr::select(npss_num, npss_22_num:npss_29_num)) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, npss_22_num:npss_29_num), na.rm = T), NA),

           npss_soc_engage_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_1_num:npss_14_num)))
                                         < ncol(dplyr::select(npss_num, npss_1_num:npss_14_num)) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, npss_1_num:npss_14_num), na.rm = T), NA),
           npss_compassion_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_15_num:npss_21_num)))
                                         < ncol(dplyr::select(npss_num, npss_15_num:npss_21_num)) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, npss_15_num:npss_21_num), na.rm = T), NA),
           npss_body_sense_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, npss_22_num:npss_29_num)))
                                         < ncol(dplyr::select(npss_num, npss_22_num:npss_29_num)) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, npss_22_num:npss_29_num), na.rm = T), NA))


  npss_subscales <- dplyr::select(npss_num, unique_id_for_merging, npss_sum, npss_mean, dplyr::ends_with('sum'), dplyr::ends_with('mean'))


  #TODO merge by unique id then move columns


  npss_scored <- merge(input_df, npss_subscales, by = 'unique_id_for_merging')



  if (upper_prefix == F){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='npss_1')

    # npss_scored <- add_column(input_df, npss_subscales, .before = 'npss_1')
  }

  if (upper_prefix == T){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='NPSS_1')

    # npss_scored <- add_column(input_df, npss_subscales, .before = 'NPSS_1')
  }





  npss_scored <- dplyr::select(npss_scored, -unique_id_for_merging)






  return(npss_scored) # Returns only input_df with added subscales. All factoring, etc. does not remain
}


utils::globalVariables(c("npss_1_num", "npss_14_num", "npss_15_num", "npss_21_num",
                         "npss_22_num", "npss_29_num", "unique_id_for_merging", "npss_sum", "npss_mean"))
