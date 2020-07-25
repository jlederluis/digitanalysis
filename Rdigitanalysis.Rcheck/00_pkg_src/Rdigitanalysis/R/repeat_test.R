############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#helper function
############################################################


#find the percent of repeats in the given data based on given definition of a repeat (what columns need to match)
find_percent_repeats = function(data, duplicate_matching_cols){

  #find repeats based on specified definition of a repeat
  columns_for_repeat_check = data[duplicate_matching_cols]
  unique_numbers = dim(unique(columns_for_repeat_check))[1]
  total_numbers = dim(columns_for_repeat_check)[1]
  num_repeats = total_numbers - unique_numbers
  percent_repeats = num_repeats / total_numbers

  return(percent_repeats)
}

#returns a table of the percentage of repeated numbers for each sector
percent_repeats_by_sector = function(data, sector_column, sector_grouping){

  #intilaize an array for the category we are analyzing on (all, district name A, etc....)
  sector_table = data.frame(matrix(nrow = length(sector_grouping), ncol=1))
  #name each sector s.t. if sector name does not match in the rows, rbind in df will throw error
  rownames(sector_table) = names(sector_grouping)

  #get indexes for each category in the specified sector column
  indexes_of_sectors = break_by_category(data, sector_column) #this is a list since unequal number of entries for each category

  for (sector_name in names(sector_grouping)){

    #get the index of sector by accessing the names of the categories in the data column that belong to this sector
    indexes_of_sector = indexes_of_sectors[sector_grouping[[sector_name]]]
    indexes_of_sector = unlist(indexes_of_sector) #turn into an array

    data_of_sector = data.frame(data[indexes_of_sector, ])

    percent_repeats_in_sector = find_percent_repeats(data_of_sector, duplicate_matching_cols)

    sector_table[sector_name, ] = percent_repeats_in_sector
  }
  return(sector_table)
}


################main function############
#performs repeat test / or sector effect test
#digitdata is the class object;
#duplicate_matching_cols are the names of columns of data needs to be matched exactly in order to be defined as a repeat
#if analysis by groups is desired, break_out should specify the deisred category to break upon
#sector_column is the column for splitting the data into sectors for separate analysis
###if it is specified (not NA), then it will perform a sectorized test instead of a normal repeat test
#sector_grouping is a list of arrays, where each element with its name in the list, represent the categories that belong to each sector in

####
#need to do failure factor
#need ADD plot parameter
####

#IF NaN is in returned table, it means that there are no occurances of the data of the sector in that category --> 0/0 in percentage

repeat_test = function(digitdata, data_columns='all', duplicate_matching_cols, break_out=NA, sector_column=NA, sector_grouping=NA, failure_factor=3){

  #check input
  input_check(digitdata=digitdata, break_out=break_out, duplicate_matching_cols=duplicate_matching_cols,sector_column=sector_column, sector_grouping=sector_grouping)

  #check if the sector columns are valid
  if (!(is.na(sector_column))){
    if (is.na(match(sector_column, colnames(digitdata@cleaned)))){
      stop('specified column to analyze sector effect is not a column in the data')
    }
    else{
      for (sector_name in names(sector_grouping)){
        if (NA %in% match(sector_grouping[[sector_name]], unique(digitdata@cleaned[[sector_column]]))){
          stop('specified category is not a category in the column specified to analyze sector effect')
        }
      }
    }
  }

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #the columns we want to analyze
  data = digitdata@cleaned[data_columns]

  #perform repeat test
  if (is.na(sector_column)){
    #percent repeats for all
    percent_repeats_all = find_percent_repeats(data, duplicate_matching_cols)

    #df to store stats
    percent_repeats_table = data.frame(all=percent_repeats_all)

    #break out by category
    if (!(is.na(break_out))){
      #get indexes for each category
      indexes_of_categories = break_by_category(data, break_out) #this is a list since unequal number of entries for each category

      #break by category for all
      for (category_name in names(indexes_of_categories)){
        indexes_of_category = indexes_of_categories[[category_name]]
        data_of_category = data.frame(data[indexes_of_category, ])
        percent_repeats_in_category = find_percent_repeats(data_of_category, duplicate_matching_cols)

        percent_repeats_table[category_name] = percent_repeats_in_category #a value
      }
    }

    #get the mean of all the values computed
    mean_percent_repeated = rowMeans(percent_repeats_table)
    percent_repeats_table['mean'] = mean_percent_repeated

    #create a rowname
    rownames(percent_repeats_table) = 'percent repeated numbers'
    #sort by decreasing rounded percentage
    percent_repeats_table = t(sort(percent_repeats_table, decreasing = TRUE))

    return(percent_repeats_table)
  }

  #perform sector test
  else {

    #initialize 2D table for stats values
    sector_repeats_table = data.frame(matrix(nrow = length(sector_grouping), ncol=0))
    #name each sector s.t. if sector name does not match in the rows, rbind in df will throw error
    rownames(sector_repeats_table) = names(sector_grouping)

    #do sector effect for all
    sector_repeats_all = percent_repeats_by_sector(data, sector_column, sector_grouping)
    #update returning table
    sector_repeats_table['all'] = sector_repeats_all


    #break out by category
    if (!(is.na(break_out))){
      #get indexes for each category
      indexes_of_categories = break_by_category(data, break_out) #this is a list since unequal number of entries for each category

      #breeak by category for all
      for (category_name in names(indexes_of_categories)){
        indexes_of_category = indexes_of_categories[[category_name]]
        data_of_category = data.frame(data[indexes_of_category, ])

        #####only difference from the if loop above
        sector_repeats_in_category = percent_repeats_by_sector(data_of_category, sector_column, sector_grouping)

        sector_repeats_table[category_name] = sector_repeats_in_category #an array
        #####
      }
    }

    #need to implement failure factor

    return(sector_repeats_table)
  }
}

