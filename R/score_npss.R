#' score_npss
#' Is able to take a dataframe containing any number of columns, as long as npss_ items exist
#'
#' @param input_df Dataframe containing the 29 NPSS items, with the prefix: 'npss' Number of item must fall at the end of column names. Ex. npss_1
#' @param missing_threshold A proportion of allowed missing data for each subscale in the NPSS (Default .5). If missingness exceeds the set threshold, function returns NA for subscale score.
#'
#' @return Input dataframe with NPSS scores added.
#' @export
#'
#' @examples
#' score_npss(test_data, missing_threshold = .4)
#' score_npss(test_data, missing_threshold = .25)
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


  # Determine presence of underscore in col_names
  if(sum(grepl('npss_d\\+', names(s))) == 29){
    underscore = T
  }else if(sum(grepl('npss\\d+', names(s))) == 29){
    underscore = F
  }else{
    warning('Column Names are in an unrecognized format. Format should be npss_#')
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
  
  ## Determine Selection Variables
  if (underscore == T){
    npss_soc_select <- names(dplyr::select(npss_num, npss_1_num:npss_14_num))
    npss_compassion_select <- names(dplyr::select(npss_num, npss_15_num:npss_21_num))
    npss_body_sense_select <- names(dplyr::select(npss_num, npss_22_num:npss_29_num))
    
  }else if (underscore == F){
    npss_soc_select <- names(dplyr::select(npss_num, npss1_num:npss14_num))
    npss_compassion_select <- names(dplyr::select(npss_num, npss15_num:npss21_num))
    npss_body_sense_select <- names(dplyr::select(npss_num, npss22_num:npss29_num))
    
  }
  
    npss_num <-
    dplyr::mutate(npss_num,
           npss_soc_engage_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))))
                                        < ncol(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, dplyr::all_of(npss_soc_select)), na.rm = T), NA),
           npss_compassion_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))))
                                        < ncol(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select)), na.rm = T), NA),
           npss_body_sense_sum = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))))
                                        < ncol(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))) * missing_threshold,
                                        rowSums(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select)), na.rm = T), NA),

           npss_soc_engage_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))))
                                         < ncol(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, dplyr::all_of(npss_soc_select)), na.rm = T), NA),
           npss_compassion_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))))
                                         < ncol(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select)), na.rm = T), NA),
           npss_body_sense_mean = ifelse(rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))))
                                         < ncol(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))) * missing_threshold,
                                         rowMeans(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select)), na.rm = T), NA))

    
  # Percent Missingness
  
  npss_num <- dplyr::mutate(npss_num, 
                npss_NApct = rowSums(is.na(dplyr::select(npss_num, dplyr::matches('npss(.*)_num')))) 
                / ncol(dplyr::select(npss_num, dplyr::matches('npss(.*)_num'))),
                npss_soc_engage_NApct = rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))))
                / ncol(dplyr::select(npss_num, dplyr::all_of(npss_soc_select))),
                npss_compassion_NApct = rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))))
                / ncol(dplyr::select(npss_num, dplyr::all_of(npss_compassion_select))),
                npss_body_sense_NApct = rowSums(is.na(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))))
                / ncol(dplyr::select(npss_num, dplyr::all_of(npss_body_sense_select))))
  

  npss_subscales <- dplyr::select(npss_num, unique_id_for_merging, npss_sum, npss_mean, npss_NApct,
                                  dplyr::starts_with('npss_soc_engage'), dplyr::starts_with('npss_compassion'), 
                                  dplyr::starts_with('npss_body_sense'))


  #TODO merge by unique id then move columns


  npss_scored <- merge(input_df, npss_subscales, by = 'unique_id_for_merging')



  if (upper_prefix == F){
    if (underscore == T){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='npss_1')
    }else if (underscore == F){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='npss1')
    }

    # npss_scored <- add_column(input_df, npss_subscales, .before = 'npss_1')
  }

  if (upper_prefix == T){
    if (underscore == T){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='NPSS_1')
    } else if (underscore == F){
    npss_scored <- dplyr::relocate(npss_scored, names(npss_subscales), .before='NPSS1')
    }

    # npss_scored <- add_column(input_df, npss_subscales, .before = 'NPSS_1')
  }





  npss_scored <- dplyr::select(npss_scored, -unique_id_for_merging)






  return(npss_scored) # Returns only input_df with added subscales. All factoring, etc. does not remain
}


utils::globalVariables(c("npss_1_num", "npss_14_num", "npss_15_num", "npss_21_num",
                         "npss_22_num", "npss_29_num", "unique_id_for_merging", "npss_sum", "npss_mean"))
