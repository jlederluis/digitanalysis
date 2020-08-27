############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Sector Test functions in this file
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
#' @param category_instance_analyzing The instance of the category to perform t test on.
#' @param remove_all_category_visualize TRUE or FALSE: If TRUE, remove visualization of 'All Category' dataset for plots
#' @inheritParams repeat_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for repeat test by sector on each category
#'   \item Plots for each break out element with variables as categories specified by \code{category_grouping} if \code{plot = TRUE or 'Save'}
#'   \item If NaN is in returned table, it means that there are no occurances of the data of the sector in that category --> 0/0 in percentage
#' }
#' @export
#'
#' @examples
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a'), 'sector 2'=c('b', 'c')))
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a, b'), 'sector 2'=c('c', 'd')),
#' duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name', failure_factor=3)
sector_test = function(digitdata, break_out, category, category_instance_analyzing, data_columns=NA, duplicate_matching_cols='all',
                       break_out_grouping=NA, category_grouping=NA, rounding_patterns_to_omit=NA, plot=TRUE, remove_all_category_visualize=FALSE){

  #check input
  input_check(digitdata=digitdata, data_columns=data_columns, break_out=break_out, break_out_grouping=break_out_grouping, duplicate_matching_cols=duplicate_matching_cols,
              category=category, category_grouping=category_grouping, rounding_patterns=rounding_patterns_to_omit, plot=plot)

  #check if the sector columns are valid
  if (is.na(match(category, colnames(digitdata@cleaned)))){
    stop('specified column to break on second division is not a column in the data!')
  }
  else{
    if (!any(is.na(unlist(category_grouping)))){
      for (category_name in names(category_grouping)){
        if (NA %in% match(category_grouping[[category_name]], unique(digitdata@cleaned[[category]]))){
          print(category_name)
          stop('specified category is not a category in the column break on second division')
        }
      }
    }
  }
  #initialize repeats table to be returned
  category_names = names(break_by_category(digitdata@cleaned, category, category_grouping))
  sector_repeats_table = data.frame(matrix(nrow = length(category_names)+1, ncol = 0))
  rownames(sector_repeats_table) = c('All', category_names) #ensure each row is fixed for a category when append
  #order rownames if they are numbers
  if (!(TRUE %in% grepl("\\D", category_names))){
    rownames(sector_repeats_table) = c('All', as.character(sort(as.numeric(colnames(p_values)))))
  }

  #intialize p values table for t test value on
  p_values = data.frame(matrix(nrow=1, ncol=0))
  rownames(p_values) = category_instance_analyzing

  #perform sector test on all
  result_all = repeat_test(digitdata = digitdata, data_columns = data_columns, duplicate_matching_cols = duplicate_matching_cols,
                           break_out = category, break_out_grouping = category_grouping, rounding_patterns_to_omit = rounding_patterns_to_omit, plot=FALSE)
  p_value = result_all$p_values[[category_instance_analyzing]]
  repeats_table = result_all$percent_repeats
  #repeats_table = repeats_table[!(rownames(repeats_table) %in% c('All')), ]

  #update table and p value
  sector_repeats_table['All'] = NA
  #return(list(a=sector_repeats_table, b=repeats_table))
  sector_repeats_table['All'][rownames(repeats_table), ] = repeats_table #match the rownames by using colnames
  p_values['All'] = format_p_values(p_value)

  #get indexes for each category in the specified sector column
  indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

  for (break_out_name in names(indexes_of_categories)){
    #index of this sector
    indexes_of_break_out = indexes_of_categories[[break_out_name]]

    #create new digitdata object for each sector
    digitdata_of_break_out = make_sub_digitdata(digitdata=digitdata, indexes=indexes_of_break_out)

    #repeats test
    result_of_break_out = repeat_test(digitdata = digitdata_of_break_out, data_columns = data_columns, duplicate_matching_cols = duplicate_matching_cols,
                                      break_out = category, break_out_grouping = category_grouping, rounding_patterns_to_omit = rounding_patterns_to_omit, plot=FALSE)
    p_value = result_of_break_out$p_values[[category_instance_analyzing]]
    repeats_table = result_of_break_out$percent_repeats
    #repeats_table = t(repeats_table[!(rownames(repeats_table) %in% c('AllBreakout')), ])

    #update table and p value
    sector_repeats_table[break_out_name] = NA
    sector_repeats_table[break_out_name][rownames(repeats_table), ] = repeats_table #match the rownames by using colnames
    p_values[break_out_name] = format_p_values(p_value)
  }
  #remove NA rows if exist
  sector_repeats_table = sector_repeats_table[rowSums(is.na(sector_repeats_table)) != ncol(sector_repeats_table), ]

  #plot
  sector_plot = 'No plot with plot=FALSE'
  if (plot != FALSE){
    #remove row for 'All' if we do not want to visualize that
    plot_data = sector_repeats_table
    if (remove_all_category_visualize){
      plot_data = sector_repeats_table[!(rownames(sector_repeats_table) %in% c('All')), ]
    }

    sector_plot = hist_2D_variables(data.frame(plot_data), data_style='row', xlab=break_out, ylab='Percent Repeats',
                                    title=paste('Sector Effect Test \n', 'Broken out by ', break_out, ', ', category, sep=''))
    if (plot == TRUE){
      dev.new()
      print(sector_plot)
    }
  }
  return(list(percent_repeats=sector_repeats_table, p_values=p_values, plot=sector_plot))
}

