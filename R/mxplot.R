#' mxplot
#'
#' plot multiple xts objects by row
#' @param ... xts or ggplot objects, or a list of ggplot objects
#' @param use_one_x_axis if true then remove the x-axis of plots but the last
#' @param theme the theme to apply
#' @param titles a vector of titles
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param size line width
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim().
#'
#' @return egg object
#' @export
#'
mxplot = function(...,
                  use_one_x_axis = TRUE,
                  theme = theme_bw(),
                  titles = NULL,
                  heights = NULL,
                  size = 0.8,
                  end_spacing = 0.1) {

  UseMethod("mxplot")

}


#' mxplot.gg
#'
#' @rdname mxplot
mxplot.gg = function(...,
                     use_one_x_axis = TRUE,
                     theme = theme_bw(),
                     titles = NULL,
                     heights = NULL,
                     size = 0.8,
                     end_spacing = 0.1) {

  plots = list(...)
  assertList(plots,types = "gg")

  mxplotList(plots,
             use_one_x_axis = use_one_x_axis,
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing)
}




#' mxplot.xts
#'
#' @rdname mxplot
mxplot.xts = function(...,
                      use_one_x_axis = T,
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
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing)
}




#' mxplot.list
#'
#' @rdname mxplot
mxplot.list = function(...,
                      use_one_x_axis = T,
                      theme = theme_bw(),
                      titles = NULL,
                      heights = NULL,
                      size = 0.8,
                      end_spacing = 0.1) {

  plots = list(...)[[1]]

  assertList(plots,types = "gg")

  mxplotList(plots,
             use_one_x_axis = use_one_x_axis,
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing)
}



#' mxplotList
#'
#' @param plots a list of ggplot objects
#' @param use_one_x_axis if true then remove the x-axis of plots but the last
#' @param theme the theme to apply
#' @param titles a vector of titles
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim().
#'
#' @import ggplot2
#' @import checkmate
#' @import purrr
#' @import egg
#' @importFrom lubridate origin
#'
mxplotList = function(plots,
                      use_one_x_axis = T,
                      theme = theme_bw(),
                      titles = NULL,
                      heights = NULL,
                      end_spacing = 0.1){


  assertList(plots,types = c("gg","ggplot"))

  if(testClass(theme, c("theme","gg"))){

    theme = fixThemeMargin(theme)
    plots = applyTheme(plots, theme)
  }

  if(use_one_x_axis & length(plots) > 1){
    plots = removeXAxisButLast(plots)
  }

  if(testCharacter(titles,min.len=1)){
    for(i in 1:length(titles)){
      plots[[i]] = plots[[i]] + ggtitle(titles[i])
    }
  }

  plots = plots %>%
    removeLegendTitle %>%
    doAlign(end_spacing = end_spacing)

  ggarrange(plots = plots, heights = heights)
}



applyTheme = function(plots,theme){
  map(plots, ~ . + theme)
}

removeXAxisButLast = function(plots){
  noXAxisTheme = theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank())
  for(i in 1:(length(plots)-1)){
    plots[[i]] = plots[[i]] + noXAxisTheme
  }
  plots
}

removeLegendTitle = function(plots){
  map(plots, ~ . +  theme(legend.title = element_blank()))
}


fixThemeMargin = function(the_theme){
  the_margin = the_theme$plot.margin
  the_margin[3] = unit(0,"pt")
  the_theme = the_theme + theme(plot.margin = the_margin)
  the_theme
}

#' doAlign
#'
#' @param plots a list of ggplot objects
#' @param end_spacing extend the space in the end, as a percentage of the size of xlim()
#' @importFrom lubridate origin
doAlign = function(plots, end_spacing = 0.1){
  min_date =(map(plots, ~ min(.$data$Index))) %>% unlist %>% as.Date(origin=lubridate::origin) %>% min
  max_date = (map(plots, ~ max(.$data$Index))) %>% unlist %>% as.Date(origin=lubridate::origin) %>% max
  diff = as.numeric((max_date - min_date))
  max_date = max_date + floor(diff * end_spacing)

  lapply(plots, function(x){
    x$coordinates$limits$x = c(min_date,max_date)
    x
  })
}

