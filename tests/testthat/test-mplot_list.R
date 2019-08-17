context("test-mxplot_list")


test_that("multiplication works", {

  library(xts)
  library(checkmate)
  library(purrr)

  date_1 = seq(as.Date("2000-01-01"),as.Date("2000-01-10"),"days")

  df_1 = data.frame(a=1:10,b=((2:11) - 0.9))
  x = xts::as.xts(df_1, order.by = date_1)
  y = xts::as.xts(df_1, order.by = date_1  + 5)

  mxplot(x,y,theme=theme_textbook())

  p1 = ggxts(x)
  p2 = ggxts(y$a)

  plot_list = list(p2,p1)

  mxplotList(list(p1,p2),theme=theme_textbook())
  expect_class(p1,"gg")

  mxplot(p1,p2,p1)
  #mxplot(x)


})
