#' mxplot
#'
#' plot multiple xts objects by row
#' @param ... arguments pass to mxplotList
#' @return NULL
#' @export
#'
mxplot = function(...){
  UseMethod("mxplot")
}


#' mxplot.gg
#'
#' plot multiple xts objects by row
#'
#' @param ... ggplot objects
#' 
#' @param use_one_x_axis if true, only the last plot show x-axis
#' @param theme the theme
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim().
#' @return NULL
#' @export
#'
mxplot.gg = function(...,
    use_one_x_axis = T,
    theme = theme_bw(),
    heights = NULL,
    end_spacing = 0.1) {

  plots = list(...)
  assertList(plots,types = "gg")
  mxplotList(plots, use_one_x_axis = use_one_x_axis,
    theme = theme,
    heights = heights,
    end_spacing = end_spacing)
}


#' mxplot.xts
#'
#' plot multiple xts objects by row
#'
#' @param ... xts objects
#' @param use_one_x_axis if true, only the last plot show x-axis
#' @param size line width
#' @param theme the theme
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim().
#' @param titles the vector of titles
#'
#' @return NULL
#' @export
#'
mxplot.xts = function(..., use_one_x_axis = T,
      theme = theme_bw(),
      titles = NULL,
      heights = NULL,
      size = 0.8,
      end_spacing = 0.1) {

  data = list(...)

  assertList(data,types = "xts")

  plots = map(data, ~ggxts(.,size = size))
  mxplotList(plots,
    use_one_x_axis = use_one_x_axis,
    theme = theme, titles = titles,
    heights = heights,end_spacing = end_spacing)
}



#' mxplotList
#'
#' @param plot_list a list of ggplot objects
#' @param use_one_x_axis if true then remove the x-axis of plots but the last
#' @param theme the theme to apply
#' @param titles a vector of titles
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim().
#' @import ggplot2
#' @import checkmate
#' @import purrr
#' @import egg
#' @importFrom lubridate origin
#'
#' @export
#'
mxplotList = function(plot_list,
  use_one_x_axis = T,
  theme = theme_bw(),
  titles = NULL,
  heights = NULL,end_spacing = 0.1){


  assertList(plot_list,types = c("gg","ggplot"))

  if(testClass(theme, c("theme","gg"))){

    theme = fixThemeMargin(theme)
    plot_list = applyTheme(plot_list, theme)
  }

  if(use_one_x_axis & length(plot_list) > 1){
    plot_list = removeXAxisButLast(plot_list)
  }

  if(testCharacter(titles,min.len=1)){
    for(i in 1:length(titles)){
      plot_list[[i]] = plot_list[[i]] + ggtitle(titles[i])
    }
  }

  plot_list = plot_list %>%
    removeLegendTitle %>%
    doAlign(end_spacing = end_spacing)

  ggarrange(plots = plot_list, heights = heights)
}



applyTheme = function(plot_list,theme){
  map(plot_list, ~ . + theme)
}

removeXAxisButLast = function(plot_list){
  noXAxisTheme = theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank())
  for(i in 1:(length(plot_list)-1)){
    plot_list[[i]] = plot_list[[i]] + noXAxisTheme
  }
  plot_list
}

removeLegendTitle = function(plot_list){
  map(plot_list, ~ . +  theme(legend.title = element_blank()))
}


fixThemeMargin = function(the_theme){
  the_margin = the_theme$plot.margin
  the_margin[3] = unit(0,"pt")
  the_theme = the_theme + theme(plot.margin = the_margin)
  the_theme
}

#' doAlign
#'
#' @param plot_list a list of ggplot objects
#' @importFrom lubridate origin
doAlign = function(plot_list, end_spacing = 0.1){
  min_date =(map(plot_list, ~ min(.$data$Index))) %>% unlist %>% as.Date(origin=lubridate::origin) %>% min
  max_date = (map(plot_list, ~ max(.$data$Index))) %>% unlist %>% as.Date(origin=lubridate::origin) %>% max
  diff = as.numeric((max_date - min_date))
  max_date = max_date + floor(diff * end_spacing)

  lapply(plot_list, function(x){
    x$coordinates$limits$x = c(min_date,max_date)
    x
  })
}

