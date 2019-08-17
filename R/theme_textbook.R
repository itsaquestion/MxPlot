#' theme_textbook
#'
#' theme_bw() + no grid + no box + no background
#' @param ... argument pass to theme_bw()
#' @return a gg theme
#' @export
#'
theme_textbook = function(...){

  theme_bw(...) + theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank()
          panel.border = element_blank(),
          panel.background = element_blank()
          )
}

