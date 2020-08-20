############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Performs sector test to analyze uneven distribution of percent repeats across sectors (supposed to be uniform).
#' A wrapper function for \code{repeat_test}.
#'
#' @param category The column for splitting the data into sectors for separate analysis. The second division (usually variables) shown in plots.
#' @param category_grouping A list of arrays, or defaulted to NA. Only effective if \code{category} is not NA.
#' \itemize{
#'   \item Each the names of the elements in the list is the category name
#'   \item Each array contains the values belonging to that category
#'   \item If it is remain as NA as default, while \code{category} is not NA, then \code{category_grouping} will default to every individual item in
#'   \code{category} will be in a separate group.
#' }
#' @inheritParams repeat_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for repeat test by sector on each category
#'   \item Plots for each break out element with variables as categories specified by \code{category_grouping} if \code{plot = TRUE}
#'   \item If NaN is in returned table, it means that there are no occurances of the data of the sector in that category --> 0/0 in percentage
#' }
#' @export
#'
#' @examples
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a'), 'sector 2'=c('b', 'c')))
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a, b'), 'sector 2'=c('c', 'd')),
#' duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name', failure_factor=3)
sector_test = function(digitdata, category, category_grouping=NA, data_columns='all', duplicate_matching_cols='digit_columns',
                       break_out=NA, break_out_grouping=NA, round_digit_to_skip=NA, plot=TRUE){

  #check input
  input_check(digitdata=digitdata, data_columns=data_columns, break_out=break_out, break_out_grouping=break_out_grouping, duplicate_matching_cols=duplicate_matching_cols,
              category=category, category_grouping=category_grouping, omit_05=round_digit_to_skip, plot=plot)

  #check if the sector columns are valid
  if (is.na(match(category, colnames(digitdata@cleaned)))){
    stop('specified column to break on second division is not a column in the data!')
  }
  else{
    if (!(is.na(category_grouping))){
      for (category_name in names(category_grouping)){
        if (NA %in% match(category_grouping[[category_name]], unique(digitdata@cleaned[[category]]))){
          print(category_name)
          stop('specified category is not a category in the column break on second division!')
        }
      }
    }
  }
  #initialize table to be returned
  category_names = c()
  if (!(is.na(break_out))){
    category_names = names(break_by_category(digitdata@cleaned, break_out, break_out_grouping))
  }
  sector_repeats_table = data.frame(matrix(nrow = length(category_names)+1, ncol = 0)) # +1 (all)
  rownames(sector_repeats_table) = c('All', category_names) #ensure each row is fixed for a category when append

  #get indexes for each category in the specified sector column
  indexes_of_sectors = break_by_category(digitdata@cleaned, category, category_grouping) #this is a list since unequal number of entries for each category

  for (sector_name in names(indexes_of_sectors)){
    print(sector_name)

    #index of this sector
    indexes_of_sector = indexes_of_sectors[[sector_name]]

    #create new digitdata object for each sector
    digitdata_of_sector = make_sub_digitdata(digitdata=digitdata, indexes=indexes_of_sector)

    #repeats test
    repeats_table = repeat_test(digitdata_of_sector, data_columns, duplicate_matching_cols, break_out, break_out_grouping, round_digit_to_skip, plot=FALSE)$percent_repeats
    #update table


    print(repeats_table)

    sector_repeats_table[sector_name] = NA
    sector_repeats_table[sector_name][rownames(repeats_table), ] = repeats_table #match the rownames by using rownames
    # return(list(a=sector_repeats_table, b=repeats_table))

    print(sector_repeats_table)
  }
  #plot
  sector_plot = NA
  if (plot){
    sector_plot = hist_2D_variables(data.frame(sector_repeats_table), data_style='col', xlab=break_out, ylab='Percent Repeats',
                                    title=paste('Sector Effect Test \n', 'break_out = ', break_out, ' \ncategory = ', category, sep=''))
    dev.new()
    print(sector_plot)
  }
  return(list(percent_repeats=sector_repeats_table, plot=sector_plot))
}

