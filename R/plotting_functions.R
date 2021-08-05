############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Plotting helper functions
############################################################

#' StackOverflow method to specify number of ticks for \code{ggplot::scale_y_continuous()}
#'
#' @param n Number of ticks desired
#'
#' @return Nothing is returned.
number_ticks <- function(n) {function(limits) pretty(limits, n)}

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
#' @param hline_name The title of the horizontal line added. Defaulted to ''.
#' @param abline A \code{ggplot2::geomline()} instance that specifies the line to be addded onto the 2D barplot.
#' @param width The width of the bars. Defaulted to 0.5.
#'
#' @return A ggplot instance.
hist_2D = function(data, data_style='row', xlab='Digits', ylab='Frequency', title='2D Barplot', hline=NA, hline_name='', abline=NA, width=0.5){
  if (data_style == 'row'){
    #transpose it to column style, what ggplot wants
    data = data.frame(t(data))
  }
  plotting_data = data.frame(x=rownames(data), y=data)
  colnames(plotting_data) = c('x', 'y') #ensure col name are correct

  #2d plot
  hist2d = ggplot(data=plotting_data, aes(x=x, y=y)) +
    geom_bar(stat="identity", width=width) + xlab(xlab) + ylab(ylab) + ggtitle(title) + scale_x_discrete(limits=rownames(data)) +
    scale_y_continuous(breaks=number_ticks(10), expand = expansion(mult = c(0, 0)), limits = c(min(0, 1.1 * min(data)), 1.1 * max(data))) + theme_bw() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(),
          legend.position="top") +
    geom_hline(aes(yintercept=0), color='black', lwd=0.5)
  #+ geom_text(aes(label=values), vjust=-0.3, size=3.5, color='blue') #for label exact value

  #add the horizontal line desired
  if (!(is.na(hline))){
    hist2d = hist2d + geom_hline(aes(yintercept=hline, linetype=hline_name), color='red', lwd=1)
  }
  #add the distribution line desired
  if ('gg' %in% class(abline)){
    hist2d = hist2d + abline
  }
  return(hist2d)
}


#' Plot 2d histogram with multiple variables as specified by typically the \code{break_out} in digit tests.
#' If \code{data} is row style, rownames would be the categories, and colnames would be x values.
#'
#' @param data The 2D dataframe to be plotted.
#' @inheritParams hist_2D
#'
#' @return A ggplot instance.
hist_2D_variables = function(data, data_style='row', xlab='Digits', ylab='Frequency', title='Multi-variable 2D Barplot', abline=NA){
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

  #stacked 2d barplot with multiple groups: use position=position_dodge()
  hist2d_multiple = ggplot(data=plotting_data, aes(x=x, y=y, fill=category)) +
    geom_bar(stat="identity", position=position_dodge()) + scale_x_discrete(limits=rownames(data)) +
    scale_fill_grey(start=0.7, end=0.1) + xlab(xlab) + ylab(ylab) + ggtitle(title) +
    scale_y_continuous(breaks=number_ticks(10), expand = expansion(mult = c(0, 0)), limits = c(min(0, 1.1 * min(data)), 1.1 * max(data))) + theme_bw() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank()) +
    geom_hline(aes(yintercept=0), color='black', lwd=0.5)

  #add the distribution line desired
  if ('gg' %in% class(abline)){
    hist2d_multiple = hist2d_multiple + abline
  }
  return(hist2d_multiple)
}

#' Plot multiple plots on a single image
#'
#' @param plot_list A list of ggplot instances
#'
#' @return A plot instance with all plots in one single figure
plot_multiple_hist2d = function(plot_list){
  plots = gridExtra::arrangeGrob(grobs = plot_list, nrow = floor(sqrt(length(plot_list))))
  return(plots)
}

#' Plot 2D histogram on digits frequency on each digit place in a single figure using \code{hist_2D} and \code{plot_multiple_hist2d}
#'
#' @param observed_table The digits table for observed counts in each digits in each digit place
#' @param expected_table The digits table for expected counts in each digits in each digit place
#' @param name The subtitle to be put on each subplot after identifying its digit place in main title
#'
#' @return A figure with each data column's value plotted against rownames
plot_table_by_columns = function(observed_table, expected_table, name=''){
  plot_list = list()
  for (i in 1:length(observed_table)){
    curr_digit_place = colnames(observed_table)[i]
    #create ggplot object for abline distribution
    dist_line = geom_line(data = data.frame(x=rownames(expected_table), y=expected_table[[i]]), aes(x = x, y = y, group=1, linetype='Expected Distribution'), color='red', lwd=1)
    hist_digit_place_i = hist_2D(observed_table[i], data_style='col', xlab='Digits', ylab='Frequency', title=paste(curr_digit_place, ' \n', name, sep=''), abline=dist_line)
    plot_list[[curr_digit_place]] = hist_digit_place_i

    #s.t. return a ggplot if it is not a multiple plot
    if (length(observed_table) == 1){
      return(hist_digit_place_i)
    }
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
#' @return Nothing is returned. Displays a \code{plot3D} 3d plot automatically.
hist_3d = function(data, digitdata, xlab='Digits', ylab='Digit Places', zlab='Frequency', title='3D Barplot', theta=55, phi=16, plot=TRUE, save3Dfilename='', kwargs=NA){
  #assert digitdata is of correct class
  
  print("3D histogram disabled due to issues in underlying package (Plot3D) as of August, 2021")

  # input_check(digitdata=digitdata)

  # x = NA
  # need_better_labels = FALSE
  # if (NA %in% as.numeric(rownames(data))){
  #   x = 1:length(rownames(data))
  #   need_better_labels = TRUE
  # }
  # else {
  #   x = as.numeric(rownames(data))
  # }

  # y = as.numeric(which(digitdata@left_aligned_column_names %in% colnames(data)))
  # z = as.matrix(data)
  # #some stuff for when x != min(x):max(x) to make 3d plot nice
  # for (i in 1:length(x)){
  #   if (!i %in% x){
  #     #new_rows = data.frame(matrix(0, ncol=ncol(z), nrow=1))
  #     z_begin = as.matrix(as.data.frame(z[1:i-1, ]))
  #     z_end = as.matrix(as.data.frame(z[i:nrow(z), ]))
  #     z = as.matrix(rbind(rbind(z_begin, rep(0, ncol(z))), z_end))
  #   }
  # }
  # x = min(x):max(x)

  # if (plot == TRUE){
  #   dev.new()
  #   bar3D = plot3D::hist3D(x=x, y=y, z=z, zlim=c(0,max(z, na.rm=TRUE)+0.01), bty = "b2", theta=theta, phi=phi, axes=TRUE, label=TRUE, nticks=max(length(x),length(y))-1,
  #                          ticktype="detailed", space=0, expand=0.5, d=2, col='grey', colvar=NA, border='black', shade=0,
  #                          lighting=list('ambient'=0.6, 'diffuse'=0.6), main=title, xlab=xlab, ylab=ylab, zlab=zlab)

  #   #xticks only when the rownames are not int
  #   if (need_better_labels){
  #     xticks = rownames(data)
  #     xlabel_pos = trans3d(x+0.2, min(y)-1.25, 0, bar3D)
  #     text(xlabel_pos$x, xlabel_pos$y, labels=xticks, adj=c(0, NA), srt=320, cex=1)
  #   }
  }

  ###these might work? for alternative labeling
  # #yticks
  # yticks = y
  # ylabel_pos = trans3d(max(x)+1, y-0.2, 0, bar3D)
  # text(ylabel_pos$x, ylabel_pos$y, labels=yticks, adj=c(0, NA), srt=0, cex=1)
  #
  # #zticks
  # zticks = seq(from=0.0, to=max(z, na.rm=TRUE)+0.01, by=round(max(z, na.rm=TRUE)/max(length(x),length(y)), digits = 1))
  # zlabel_pos = trans3d(min(x)-1, min(y)-1, zticks, bar3D)
  # text(zlabel_pos$x, zlabel_pos$y, labels=zticks, adj=c(0, NA), srt=0, cex=1)
  # plot3D::text3D(x = 1:length(x)+0.3, y = rep(1.15, length(x)), z = rep(0, length(x)), labels = x, add = TRUE, adj = 0)
  # plot3D::text3D(x = rep(0, length(y)), y = 1:length(y)+0.5, z = rep(1, length(y)), labels = y, add = TRUE, adj = 1)

  if (save3Dfilename != ''){
    filename = paste(gsub('\n', '', title), save3Dfilename, ".pdf", sep='')
    pdf(file = filename)
    plot3D::hist3D(x=x, y=y, z=z, zlim=c(0,max(z, na.rm=TRUE)+0.01), bty = "b2", theta=theta, phi=phi, axes=TRUE, label=TRUE, nticks=max(length(x),length(y)),
                   ticktype="detailed", space=0, expand=0.5, d=2, col='grey', colvar=NA, border='black', shade=0,
                   lighting=list('ambient'=0.6, 'diffuse'=0.6), main=title, xlab=xlab, ylab=ylab, zlab=zlab)#, cex.axis = 1e-9)
    dev.off()
  }
}

#' Plot aggregated histogram for each digit weighted average across each digit place with expected distribution.
#'
#' @param observation_table Observation table for chi square test in frequency.
#' @param expected_table Expected table for chi square test in frequency.
#' @param freq_digit_place The frequency of each digit place in data. N in digit place i / total N.
#' @param name Title for the plot.
#'
#' @return A ggplot instance for aggregated histogram.
plot_aggregate_histogram = function(observation_table, expected_table, freq_digit_place, name){
  if (abs(sum(freq_digit_place) - 1) > 0.001){
    message = paste('freq_digit_place must sum to 1 to be a weight vector: In ', name,
                    ', sum of frequency for all digits - 1 = ', sum(freq_digit_place)-1, sep='')
    stop(message)
  }
  aggregate_expected = data.frame(matrix(nrow=0, ncol=1))
  colnames(aggregate_expected) = 'Aggregate Expected Frequency'
  aggregate_observed = data.frame(matrix(nrow=0, ncol=1))
  colnames(aggregate_observed) = 'Aggregate Observed Frequency'

  for (i in 1:nrow(observation_table)){
    aggregate_expected[rownames(observation_table)[i], ] = sum(freq_digit_place * expected_table[i, ])
    aggregate_observed[rownames(observation_table)[i], ] = sum(freq_digit_place * observation_table[i, ])
  }
  aggregate_plot = plot_table_by_columns(aggregate_observed, aggregate_expected, name=name)
  return(aggregate_plot)
}


#' Plots the relevant plots for obseravtion table in \code{all_digits_test}.
#'
#' @param observation_table Observation table for chi square test
#' @param expected_table Expected table for chi square test
#' @param title The title for the plot after automatically generating the name for the test: either single digit test or all digit test.
#' @inheritParams all_digits_test
#'
#' @return Nothing is returned. Displays plots automatically.
plot_all_digit_test = function(digitdata, observation_table, expected_table, digit_places, title='', plot=TRUE, save3Dfilename='', kwargs=NA){
  plots_list = list()
  test_type = NA
  freq_digit_place = data.frame(t(colSums(observation_table))) / sum(observation_table) # for aggregate histogram

  #turn observation table from counts into frequency
  for (i in 1:length(observation_table)){
    observation_table[, i] = observation_table[, i] / sum(observation_table[, i], na.rm = TRUE)
    expected_table[, i] = expected_table[, i] / sum(expected_table[, i], na.rm = TRUE)
  }
  if (length(digit_places) == 1){
    test_type = 'Single Digit Test'
    aggregate_hist = plot_table_by_columns(observation_table, expected_table, name=paste(test_type, ' \n', title, sep='')) #multiple 2D histograms
    plots_list[['aggregate_barplot']] = aggregate_hist
    if (plot == TRUE){
      dev.new()
      print(aggregate_hist)
    }
  }
  #unpack rounded test
  else if (dim(digitdata@raw) != c(0,0) && digitdata@raw %in% c('round', 'unround')){
    #round numbers
    if (digitdata@raw[1,1] == 'round'){
      aggregate_hist = plot_aggregate_histogram(observation_table, expected_table, freq_digit_place, name=paste('Rounded Data \n', title, sep='')) #plot aggregate histogram across digit place
      plots_list[['aggregate_barplot']] = aggregate_hist

    }
    else if (digitdata@raw[1,1] == 'unround'){
      aggregate_hist = plot_aggregate_histogram(observation_table, expected_table, freq_digit_place, name=paste('Unround Data \n', title, sep='')) #plot aggregate histogram across digit place
      plots_list[['aggregate_barplot']] = aggregate_hist
    }
    else {
      stop('Error happened in plot_all_digit_test in plotting_functions.R. Do traceback() or look at plotting_functions.R (note for developer).')
    }
  }
  else {
    test_type = 'All Digit Test'
    hist_3d(observation_table, digitdata, xlab='Digits', ylab='Digit Places', zlab='Frequency', title=paste(test_type, ' \n', title, sep=''), plot=plot, save3Dfilename=save3Dfilename, kwargs=kwargs) #3D histogram
    multiple_hist = plot_table_by_columns(observation_table, expected_table, name=paste(test_type, ' \n', title, sep='')) #multiple 2D histograms
    aggregate_hist = plot_aggregate_histogram(observation_table, expected_table, freq_digit_place, name=paste(test_type, ' \n', title, sep='')) #plot aggregate histogram across digit place
    plots_list[['digitplace_barplot']] = multiple_hist
    plots_list[['aggregate_barplot']] = aggregate_hist
  }
  return(plots_list)
}

