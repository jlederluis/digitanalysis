############################################################
#Functions for digit analysis R package
###plotting helper functions
#Wenjun Chang
#Summer 2020
############################################################


#' Plot 2d histogram given data. Either rownames or colnames will be x values, and the data will be y values.
#'
#' @param data The 1D dataframe to be plotted.
#' @param data_style The style of input \code{data}. \code{ggplot} requires columns to be the arrays for x values, y values, etc.
#' \itemize{
#'   \item If \code{data} has a row for y values, pass in 'row'.
#'   \item If \code{data} has a column for y values, pass in 'col'.
#'   \item Defaulted to 'row'.
#' }
#'
#' @param xlab x-axis label. Defaulted to 'digits'.
#' @param ylab y-axis label. Defaulted to 'frequency'.
#' @param title Plot title. Defaulted to '2D Histogram'.
#' @param hline Specifies the y-intercept value if a horizontal line is desired. Defaulted to NA.
#'
#' @return A ggplot instance.
hist_2D = function(data, data_style='row', xlab='digits', ylab='frequency', title='2D Histogram', hline=NA){
  if (data_style == 'row'){
    #transpose it to column style, what ggplot wants
    data = data.frame(t(data))
  }
  plotting_data = data.frame(x=rownames(data), y=data)
  colnames(plotting_data) = c('x', 'y') #ensure col name are correct

  #2d plot
  library(ggplot2)
  hist2d = ggplot(data=plotting_data, aes(x=x, y=y)) +
    geom_bar(stat="identity") + xlab(xlab) + ylab(ylab) + ggtitle(title)
  #+ geom_text(aes(label=values), vjust=-0.3, size=3.5, color='blue') #for label exact value

  if (!(is.na(hline))){
    #add the horizontal line desired
    hist2d = hist2d + geom_hline(yintercept=hline, color='red')
  }
  return(hist2d)
}


#' Plot 2d histogram with multiple varibales as specified by typically the \code{break_out} in digit tests.
#' If \code{data} is row style, rownames would be the categories, and colnames would be x values.
#'
#' @param data The 2D dataframe to be plotted.
#' @inheritParams hist_2D
#'
#' @return A ggplot instance.
hist_2D_variables = function(data, data_style='row', xlab='digits', ylab='frequency', title='Multi-variable 2D Histogram'){
  if (data_style == 'row'){
    #transpose it to column style, what ggplot wants
    data = data.frame(t(data))
  }
  #intialize a df for plotting, columns = x, y, category
  plotting_data = data.frame(matrix(nrow = 0, ncol = 3))
  colnames(plotting_data) = c('x', 'y', 'category')

  #fill up df
  for (name in colnames(data)){
    single_category_data = data.frame(x=rownames(data), y=data[[name]], category=rep(name, nrow(data)))
    plotting_data = rbind(plotting_data, single_category_data)
  }

  #stacked 2d barplot with multiple groups
  #use position=position_dodge()
  library(ggplot2)
  hist2d_multiple = ggplot(data=plotting_data, aes(x=x, y=y, fill=category)) +
    geom_bar(stat="identity", position=position_dodge()) + scale_x_discrete(limits=rownames(data)) +
    xlab(xlab) + ylab(ylab) + ggtitle(title) #ensure order of digit places
  #+ theme(legend.position="bottom") #legend position
  return(hist2d_multiple)
}

#' Plot multiple plots on a single image
#'
#' @param plot_list A list of ggplot instances
#'
#' @return A plot instance with all plots in one single figure
plot_multiple_hist2d = function(plot_list){
  require(gridExtra)
  plots = do.call("grid.arrange", c(plot_list, nrow = floor(sqrt(length(plot_list)))))
  return(plots)
}


#' Plot 2D histogram on digits freqency on each digit place in a single figure using \code{hist_2D} and \code{plot_multiple_hist2d}
#'
#' @param digits_table The digits table for counts in each digits in each digit place
#' @inheritParams hist_2D
#'
#' @return A figure with each data column's value plotted against rownames
plot_all_digit_places = function(digits_table, name='', data_style='col'){
  plot_list = list()
  #turn into frequency decimal
  for (i in 1:length(digits_table)){
    digits_table[, i] = digits_table[, i] / sum(digits_table[, i], na.rm = TRUE)
  }
  for (i in 1:length(digits_table)){
    curr_digit_place = colnames(digits_table)[i]
    hist_digit_place_i = hist_2D(digits_table[i], data_style=data_style, xlab='digits', ylab='frequency', title=paste(curr_digit_place, name), hline=NA)
    plot_list[[curr_digit_place]] = hist_digit_place_i
  }
  plots = plot_multiple_hist2d(plot_list)
  return(plots)
}


#' Plot 3D histogram. The display follows that rows will be x-axis, and columns will be y-axis.
#'
#' @param data The 2D dataframe to be plotted.
#' @param zlab z-axis label. Defaulted to 'frequency'.
#' @param theta x-y (left-right) viewing angle. Defaulted to 55.
#' @param phi xy-z (up-down) viewing angle. Defaulted to 16.
#' @inheritParams hist_2D
#' @inheritParams all_digits_test
#'
#' @return Nothing is retuned. Displays a \code{plot3D} 3d plot automatically.
hist_3d = function(data, digitdata, xlab='digits', ylab='digit places', zlab='frequency', title='3D Bar Plot', theta=55, phi=16){
  #assert digitdata is of correct class
  input_check(digitdata=digitdata)

  #turn into frequency decimal
  for (i in 1:length(data)){
    data[, i] = data[, i] / sum(data[, i], na.rm = TRUE)
  }
  x = as.numeric(rownames(data))
  y = as.numeric(which(digitdata@left_aligned_column_names %in% colnames(data)))
  z = as.matrix(data)
  plot3D::hist3D(x=x, y=y, z=z, zlim=c(0,max(z, na.rm=TRUE)+0.01), theta=theta, phi=phi, axes=TRUE, label=TRUE, nticks=max(length(x),length(y)),
                 ticktype="detailed", space=0, expand=0.6, d=2, col='grey', colvar=NA, border='black', shade=0,
                 lighting=list('ambient'=0.6, 'diffuse'=0.6), main=title, xlab=xlab, ylab=ylab, zlab=zlab)
}

