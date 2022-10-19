
col_plot <- function(data, x, y, group, mult = 1, Legend = F, text_vjust = 0, text_col = "black", 
                     text_fface = "bold", text.size = 3.5, facet_nrow = 2, facet_strip = "right", 
                     Breaks = waiver(), col_man = rep("black", facet_nrow), 
                     Fill_man = rep("thistle2", facet_nrow), Title = "Title", Subtitle = waiver(), 
                     x.lab = "x-axis label", y.lab = "y-axis label", Round = 2, Angle = 0) {
  x = parse(text = x)
  y = parse(text = y)
  group = parse(text = group)
  
  col <- ggplot(data, aes(x = eval(x), y = eval(y)*mult)) +
    geom_col(aes(color = eval(group), fill = eval(group)), show.legend = Legend) +
    geom_text(aes(label = round(eval(y)*mult, Round)), 
              vjust = text_vjust, 
              colour = text_col, 
              fontface = text_fface, 
              size = text.size) +
    facet_wrap(~eval(group), nrow = facet_nrow, strip.position = facet_strip) +
    scale_y_continuous(breaks = Breaks) +
    scale_color_manual(values = col_man) +
    scale_fill_manual(values = Fill_man) +
    theme(axis.text.x = element_text(angle = Angle)) +
    ggtitle(Title, Subtitle) +
    xlab(x.lab) +
    ylab(y.lab)
  
  return(col)
}


map_plot <- function(data, lng, lat, group, k = 0.01, Zoom = 11, Type = "toner-lite", transp = 0.05,
                     dot.size = 0.85, col_man = "black", fill_man = "black", x.lab = "longitude",
                     y.lab = "latitude", Title = "map") {
  lng = parse(text = lng)
  lat = parse(text = lat)
  group = parse(text = group)
  
  map.lim = c(min(data[, eval(lng)]) - k, min(data[, eval(lat)]) - k, 
              max(data[, eval(lng)]) + k, max(data[, eval(lat)]) + k)
  map = get_stamenmap(bbox = map.lim, zoom = Zoom, maptype = Type)
  
  map.plot <- ggmap(map) +
    geom_point(data = data, 
               aes(x = eval(lng), y = eval(lat), fill = eval(group), colour = eval(group)),
               alpha = transp, size = dot.size) +
    scale_color_manual(values = col_man) +
    scale_fill_manual(values = fill_man) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
    theme(legend.title = element_blank(), legend.position = c(0.80, 0.90),
          legend.background = element_rect(colour = "black", size = 0.1)) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(label = Title)
  
  return(map.plot)
}


get_cols <- function(data) {
  ls = vector(mode = "list", length = 3)
  names(ls) <- c("nr.ym", "nr.wd", "nr.rt")
  
  ls$nr.ym <- col_plot(data$rl_summ$ym, "year_month", "obs", "member_casual", mult = 1/1000,
                       text_vjust = 1.2, Breaks = seq(0, 500, 50), Title = "Num. rides by month",
                       Fill_man = viridisLite::turbo(25)[c(5,16)], x.lab = "Month",
                       y.lab = "Num. rides (thousands)", Round = 0, Angle = 90)
  ls$nr.wd <- col_plot(data$rl_summ$wd, "weekday", "obs", "member_casual", mult = 1/1000,
                       text_vjust = 5, Breaks = seq(0, 500, 50), Title = "Num. rides by weekday",
                       Fill_man = viridisLite::turbo(25)[c(5,16)], x.lab = "Weekday",
                       y.lab = "Num. rides (thousands)", Round = 0)
  
  ls$nr.rt <- ggplot(data$rt_summ, aes(x = rideable_type, y = obs/1000)) +
    geom_col(aes(fill = member_casual), position = "dodge", colour = "black") +
    geom_text(
      aes(group = member_casual, label = round(obs/1000)), 
      position = position_dodge2(width = 1),
      vjust = 2, fontface = "bold"
    ) +
    scale_fill_manual(values = viridisLite::turbo(25)[c(5,16)]) +
    theme(legend.position = c(.85, .85), legend.title = element_blank()) +
    ylab("Num. rides (thousands)") +
    xlab("Bike type") + 
    ggtitle("Num. rides by bike type")
  
  return(ls)
}


get_maps <- function(data) {
  ls <- vector(mode = "list", 2)
  names(ls) <- c("start_map", "end_map")
  ls$start_map <- map_plot(data, "start_lng", "start_lat", "member_casual",
                           col_man = viridisLite::turbo(25)[c(5,16)], 
                           fill_man = viridisLite::turbo(25)[c(5,16)], Title = "Start map")
  ls$end_map <- map_plot(data, "end_lng", "end_lat", "member_casual",
                         col_man = viridisLite::turbo(25)[c(5,16)], 
                         fill_man = viridisLite::turbo(25)[c(5,16)], Title = "End map")
  return(ls)
}
