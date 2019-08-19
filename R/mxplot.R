#' mxplot
#'
#' Plot multiple xts objects by row.
#' Auto align the range of xlim of all plots.
#' Show the latest value of xts objects.
#' @param ... xts or ggplot objects, or a list of ggplot objects. If it's a list, only the 1st item works.
#' @param use_one_x_axis if true then remove the x-axis of plots but the last
#' @param theme the theme to apply
#' @param titles a vector of titles.
#' @param heights the weight of the height of plots, eg. heights = c(2,1)
#' @param size line width
#' @param end_spacing extend the space at the end of plot, as a percentage of the size of xlim().
#' @param vlines add multiple geom_vline(). eg vlines = c("2000-01-05","2000-01-13").
#' @param vlines_color color of vlines
#'
#' @return an egg object.
#' @export
#'
mxplot = function(...,
                  use_one_x_axis = TRUE,
                  theme = theme_bw(),
                  titles = NULL,
                  heights = NULL,
                  size = 0.8,
                  end_spacing = 0.1,
                  vlines = NULL,
                  vlines_color = "gray50") {

  UseMethod("mxplot")

}


#' mxplot.gg
#' @export
#' @rdname mxplot
mxplot.gg = function(...,
                     use_one_x_axis = TRUE,
                     theme = theme_bw(),
                     titles = NULL,
                     heights = NULL,
                     size = 0.8,
                     end_spacing = 0.1,
                     vlines = NULL,
                     vlines_color = "gray50") {

  plots = list(...)
  assertList(plots,types = "gg")

  mxplotList(plots,
             use_one_x_axis = use_one_x_axis,
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing,
             vlines = vlines,
             vlines_color=vlines_color)
}




#' mxplot.xts
#' @export
#' @rdname mxplot
mxplot.xts = function(...,
                      use_one_x_axis = T,
                      theme = theme_bw(),
                      titles = NULL,
                      heights = NULL,
                      size = 0.8,
                      end_spacing = 0.1,
                      vlines = NULL,
                      vlines_color = "gray50") {

  data = list(...)

  assertList(data,types = "xts")

  plots = map(data, ~ggxts(.,size = size))

  mxplotList(plots,
             use_one_x_axis = use_one_x_axis,
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing,
             vlines = vlines,
             vlines_color=vlines_color)
}




#' mxplot.list
#' @export
#' @rdname mxplot
mxplot.list = function(...,
                      use_one_x_axis = T,
                      theme = theme_bw(),
                      titles = NULL,
                      heights = NULL,
                      size = 0.8,
                      end_spacing = 0.1,
                      vlines = NULL,
                      vlines_color = "gray50") {

  plots = list(...)[[1]]

  assertList(plots,types = "gg")

  mxplotList(plots,
             use_one_x_axis = use_one_x_axis,
             theme = theme,
             titles = titles,
             heights = heights,
             end_spacing = end_spacing,
             vlines = vlines,
             vlines_color=vlines_color)
}



#' mxplotList
#' @import egg
#' @import purrr
#' @import ggplot2
#' @import checkmate
#' @importFrom lubridate origin
mxplotList = function(plots,
                      use_one_x_axis = T,
                      theme = theme_bw(),
                      titles = NULL,
                      heights = NULL,
                      end_spacing = 0.1,
                      vlines = NULL,
                      vlines_color = "gray50"){


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

  if(!is.null(vlines)){
    plots = map(plots, ~ . + geom_vline(xintercept = as.Date(vlines),col = vlines_color))
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
#' @import purrr
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

