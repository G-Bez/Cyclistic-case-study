
box_plot <- function(data, x, y, mult=1, Fill = "thistle2", outl.size = 0.2, Notch = F, Coef = 1.5, 
                     stat = "mean", Breaks = waiver(), Title = "Title", Subtitle = waiver(), 
                     x.lab = "x-axis label", y.lab = "y-axis label") {
  
  x = parse(text = x)
  y = parse(text = y)
  
  box <- ggplot2::ggplot(data, aes(x = eval(x), y = eval(y)*mult)) +
    geom_boxplot(outlier.size = outl.size, fill = Fill, notch = Notch, coef = Coef) +
    stat_summary(fun = stat) +
    scale_y_continuous(breaks = Breaks) +
    ggtitle(Title, Subtitle) +
    xlab(x.lab) +
    ylab(y.lab)
  
  return(box)
}


Summ <- function(data, var, group_by, order_by = group_by) {
  if(!("data.table" %in% class(data))) stop("dt is not a data.table object.")
  
  x = parse(text = var)
  summ = data[
    ,
    .(obs = NROW(eval(x)), quart1 = quantile(eval(x), 0.25), mean = mean(eval(x)), 
      median = median(eval(x)), quart3 = quantile(eval(x), 0.75), iqr = IQR(eval(x)),
      stdev = sd(eval(x))), 
    by = group_by
  ] |> data.table::setorderv(cols = order_by)
  
  return(summ)
}


welch <- \() {}   ### empty functions. Purpose: show welch's tests and cohen's d stats in
cohensD <- \() {} ### pipeline graph


get_box <- function(data) {
  if(!("data.table" %in% class(data))) stop("dt is not a data.table object.")
  
  ls = vector(mode = "list", 2)
  names(ls) <- c("rl_box", "rd_box")
  
  ls$rl_box <- box_plot(data, "member_casual", "ride_length", mult = 1/60,
                        Fill =  viridisLite::turbo(25)[c(5,16)], Notch = T, Breaks = seq(0,90,5), 
                        Title = "Ride length boxplots", x.lab = "Membership",
                        y.lab = "Ride length (min.)")
  ls$rd_box <- box_plot(data, "member_casual", "ride_dist", mult = 1/1000,
                        Fill =  viridisLite::turbo(25)[c(5,16)], Notch = T, Breaks = seq(0,35,2.5), 
                        Title = "Ride dist boxplots", x.lab = "Membership", 
                        y.lab = "Ride distance (km)")
  
  return(ls)
}


get_stats <- function(data) {
  if(!("data.table" %in% class(data))) stop("dt is not a data.table object.")
  
  ls = vector(mode = "list", 5)
  names(ls) <- c("rl_summ", "rd_summ", "rt_summ", "welch", "cohensD")
  
  ls$rl_summ$ym <- Summ(data, "ride_length", c("member_casual", "year_month"))
  ls$rl_summ$wd <- Summ(data, "ride_length", c("member_casual", "weekday"))
  ls$rl_summ$mc <- Summ(data, "ride_length", "member_casual")
  ls$rd_summ <- Summ(data, "ride_dist", "member_casual")
  ls$rt_summ <- data[, .(obs = .N), by = c("member_casual", "rideable_type")]
  
  welch()
  ls$welch$rl <- t.test(ride_length ~ member_casual, data = data)
  ls$welch$rd <- t.test(ride_dist ~ member_casual, data = data)
  
  cohensD()
  ls$cohensD$rl <- rstatix::cohens_d(ride_length ~ member_casual, data = data)
  ls$cohensD$rd <- rstatix::cohens_d(ride_dist ~ member_casual, data = data)
  
  return(ls)
}
