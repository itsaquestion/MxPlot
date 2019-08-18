#' ggxts
#'
#' plot xts with end values via ggplot
#' @param x the xts object
#' @param digits digits of ene values
#' @param size line width
#' @import ggplot2
#' @import checkmate
#' @importFrom reshape2 melt
#' @importFrom  dplyr group_by
#' @importFrom  dplyr mutate
#' @import directlabels
#' @import xts
#' @import zoo
#'
#' @return ggplot object
#' @export
#'
ggxts = function(x, digits = 2,size = 0.8){
  assertClass(x,"xts")

  df = data.frame(Index = index(x), data.frame(x))

  # 加一列last_value，用于directlabels显示最新值
  df2 = reshape2::melt(df, "Index") %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(last_value = round(last(value), digits))

  ggplot(df2, aes(x = Index, y = value, group = variable, color = variable)) +
          geom_line(size = size) + ylab(NULL) + xlab(NULL) +
          geom_dl(aes(label = last_value, color = variable),
          method = list(dl.trans(x = x + 0.1), "last.qp")) +
    theme(legend.title = NULL)
}

