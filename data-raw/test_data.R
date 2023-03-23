## code to prepare `test_data` dataset goes here

response_list <- c('Strongly Disagree', 'Disagree', 'Neither Agree Nor Disagree', 'Agree', 'Strongly Agree')

test_data <- data.frame(npss_1 = sample(response_list, 10, replace=TRUE),
                         npss_2 = sample(response_list, 10, replace=TRUE),
                         npss_3 = sample(response_list, 10, replace=TRUE),
                         npss_4 = sample(response_list, 10, replace=TRUE),
                         npss_5 = sample(response_list, 10, replace=TRUE),
                         npss_6 = sample(response_list, 10, replace=TRUE),
                         npss_7 = sample(response_list, 10, replace=TRUE),
                         npss_8 = sample(response_list, 10, replace=TRUE),
                         npss_9 = sample(response_list, 10, replace=TRUE),
                         npss_10 = sample(response_list, 10, replace=TRUE),
                         npss_11 = sample(response_list, 10, replace=TRUE),
                         npss_12 = sample(response_list, 10, replace=TRUE),
                         npss_13 = sample(response_list, 10, replace=TRUE),
                         npss_14 = sample(response_list, 10, replace=TRUE),
                         npss_15 = sample(response_list, 10, replace=TRUE),
                         npss_16 = sample(response_list, 10, replace=TRUE),
                         npss_17 = sample(response_list, 10, replace=TRUE),
                         npss_18 = sample(response_list, 10, replace=TRUE),
                         npss_19 = sample(response_list, 10, replace=TRUE),
                         npss_20 = sample(response_list, 10, replace=TRUE),
                         npss_21 = sample(response_list, 10, replace=TRUE),
                         npss_22 = sample(response_list, 10, replace=TRUE),
                         npss_23 = sample(response_list, 10, replace=TRUE),
                         npss_24 = sample(response_list, 10, replace=TRUE),
                         npss_25 = sample(response_list, 10, replace=TRUE),
                         npss_26 = sample(response_list, 10, replace=TRUE),
                         npss_27 = sample(response_list, 10, replace=TRUE),
                         npss_28 = sample(response_list, 10, replace=TRUE),
                         npss_29 = sample(response_list, 10, replace=TRUE))


test_data[8,sample(ncol(test_data),12)] <- NA
test_data[9,sample(ncol(test_data),15)] <- NA
test_data[10,sample(ncol(test_data),20)] <- NA
  
# Add Subject ID
test_data <- data.frame(SubjectID = 1:10, test_data)

usethis::use_data(test_data, overwrite = TRUE)
