### Includes a bunch of scripts to make random shapes using ggplot
### you should load in the following packages: "tidyverse", "ggforce", "colortools", and "bezier" 

library(tidyverse)
library(ggforce)
library(bezier)
library(colortools)


### this function is used throughout to create n random colors, to set 'alpha = t" for variable opacity

rnd_colors <- function(n, alpha = F){
  random_colors <- vector("character", n)
  if(alpha == F){
    for(i in 1:n){
      random_colors[i] <- rgb(runif(1), runif(1), runif(1))
    }
  }
  else {
    for(i in 1:n){
      random_colors[i] <- rgb(runif(1), runif(1), runif(1), runif(1))
    }
  }  
  random_colors
}

# spiral makers -----------------------------------------------------------

### these functions draws a bunch of spirals. 
### rnd_spirals_uni_size draws spirals with the same size at different coordinates
### rnd_spirals_diff_size maker_diff_size draws spirals of different colors, sizes, and winding
### for both functions, the x and y coordinates are randomly generated using runif()

rnd_spirals_uni_size <- function(n = 10, winding = 5, size = 10, color = "black"){
  
  #generate the data
  
  tbl <- tibble()
  for(i in seq_along(1:n)){
    x_offset <- runif(1, -100, 100) 
    y_offset <- runif(1, -100, 100)
    coords <- tibble(theta = rep(seq(0, 2*pi, length.out = 1000), winding), 
                     l = seq(0, size, length.out = 5000), 
                     x = l*cos(theta)+x_offset, 
                     y = l*sin(theta)+y_offset,
                     group = as.factor(i))
    tbl <- rbind(tbl, coords)
  }
  
  #draw the spirals
  
  ggplot(data = tbl, aes(x, y))+
    geom_path(aes(group = group, color = group), show.legend = F)+
    coord_fixed()+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}

# test the function

rnd_spirals_uni_size(n = 7, size = 15)



### spirals with different sizes

rnd_spirals_diff_size <- function(n = 10, winding = 5, size = 10){
  
  #generate the data
  
  tbl <- tibble()
  winding <- abs(floor(rnorm(n, 5, 1)))
  thickness <- rnorm(n, 0.1, 0.1)
  for(i in seq_along(1:n)){
    x_offset <- runif(1, -100, 100) 
    y_offset <- runif(1, -100, 100)
    coords <- tibble(theta = rep(seq(0, 2*pi, length.out = 1000), winding[i]), 
                     l = seq(0, runif(1, 1, 30), length.out = winding[i]*1000), 
                     x = l*cos(theta)+x_offset, 
                     y = l*sin(theta)+y_offset,
                     group = as.factor(i), 
                     thickness = thickness[i])
    tbl <- rbind(tbl, coords)
  }
  
  # draw the circles
  
  ggplot(data = tbl, aes(x, y))+
    geom_path(aes(color = group), show.legend = F)+
    coord_fixed()+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
  
}

#test the function

rnd_spirals_diff_size()



# random circles ----------------------------------------------------------

### these two functions create random circles
### rnd_circles_simple makes pairs of concentric circles
### rnd_cirlces_bg creates circles in the back ground as well


rnd_circles_simple <- function(n = 20, mean_x = 0, sd_x = 1,
                             mean_y = 0, sd_y = 1,
                             mean_r = 1/2, sd_r = 1/2,
                             max_radius_difference = 1/2, alpha = T)
  {
  
  lower_circles <- tibble(
    id = 1:n,
    x0 = rnorm(n, mean_x, sd_y), 
    y0 =  rnorm(n, mean_y, sd_y), 
    r = runif(n, mean_r, sd_r), 
    fill = rnd_colors(n, alpha = alpha)
  )
  
  top_circles <- lower_circles %>% 
    mutate(r = r- runif(n, 0, max_radius_difference), 
           fill = rnd_colors(n, alpha = alpha))
  
  all_circles <- rbind(lower_circles, top_circles) %>% 
    arrange(id, desc(r))
  
  
  
  ggplot(mapping = aes(linetype = NA), data = all_circles)+
    geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = fill), show.legend = F)+
    coord_fixed()+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())+
    scale_fill_manual(values = all_circles$fill)
}

#test the function

rnd_circles_simple()


### circles of different sizes with background circles
### find a way to randomize the color of foreground circles

rnd_circles_bg <- function(n = 40, n_bg = 10) {
  
  #set the color pallet
  
  circle_colors <- c("a"="grey40",
             "b"="grey50",
             "c"="grey60",
             "d"="grey70",
             "e"="grey80",
             "f"="grey90",
             "g" = "white",
             "h" = "black",
             "A"= "#40c0ff", "B"="#E69F00", "C" = "#56B4E9",
             "D"= "#009E73","E" = "#F0E442","F" = "#0072B2", 
             "G" ="#D55E00","H"= "#CC79A7", "I" = "#ffc066", 
             "J" = "550022", "K"= '440044')
  
  
  #generate the background circles
  
  background <- tibble(
    id = 1:(n_bg),
    x0 = rnorm(n_bg, -50, 50), 
    y0 =  runif(n_bg, -200, 200), 
    r = runif(n_bg, 25, 100), 
    fill = as.factor(sample(letters[1:8], n_bg, replace = T)), 
    size = runif(n_bg, 0.2, 7), 
    color = as.factor(sample(letters[1:8], n_bg, replace = T)), 
    alpha = runif(n_bg, 0.8, 1))
  
  #generate the bottom circles
  
  lower_circles <- tibble(
    id = 1:n,
    x0 = runif(n, -50, 50), 
    y0 =  runif(n, -50, 50), 
    r = runif(n, 2, 10), 
    fill = as.factor(sample(LETTERS[1:11], n, replace = T)))
  
  #generate the top cirlcles
  
  top_circles <- lower_circles %>% 
    mutate(r = r- runif(n, 0.5, 5), 
           fill = as.factor(sample(LETTERS[1:11], n, replace = T)))
  
  #combine top and bottom  circles
 foreground_circles <- rbind(lower_circles, top_circles) %>% 
    arrange(id, desc(r))
 
 ggplot()+
   geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = fill, size = size, color = color),
               show.legend = F, data = background, n = 3600, alpha = 0.5)+
   geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = fill, linetype = NA),
               show.legend = F, alpha = 0.7, data = foreground_circles)+
   coord_fixed(xlim = c(min(lower_circles$x0)-10, max(lower_circles$x0)+10),
               ylim = c(min(lower_circles$y0)-10, max(lower_circles$y0)+10))+
   theme(panel.background = element_blank(), 
         panel.grid = element_blank(), 
         axis.title = element_blank(), 
         axis.ticks = element_blank(), 
         axis.text = element_blank())+
   scale_fill_manual(values = circle_colors)+
   scale_color_manual(values = circle_colors)
 
}

#test the function

rnd_circles_bg()

                          
# random waves ------------------------------------------------------------

### this function fits splines to random points to create random waves

rnd_waves <- function(n = 10,
                      min_y = -1, max_y = 1){

  # generate control points
  
  control_points <- tibble(x = numeric(), y = numeric(), group =factor())
  for(i in 1:n){
    temp <- tibble(
      x = c(0:14), 
      y = c(0, runif(1, min_y/10, max_y/10), runif(1, min_y/5, max_y/5),  runif(9, -1, 1), 
            runif(1,  min_y/5, max_y/5), runif(1, min_y/10, max_y/10), 0), 
      group = as.factor(i),
      color = rnd_colors(15)
    )
    control_points <- rbind(control_points, temp)
  }

  
  ggplot(control_points)+
    geom_bspline(aes(x, y, color = color, group = group), size = 3, alpha = 0.5,  n = 1000, lineend = "round", show.legend = F)+
    geom_bspline(aes(x, y, color = color, group = group),  size =1 , n = 1000, lineend = "round", show.legend = F)+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}

#test the function
rnd_waves()



# random walk trees -------------------------------------------------------

theta_runif <- function(n = 1, min = 0, max = 2*pi, ...){
  (runif(n, min, max))
}

theta_rnorm <- function(mean = pi, sd = pi/2, ...){
  (rnorm(1, mean = mean, sd = sd))
}



#random walk keeping step length constant
rw_control_points <- tibble(x = numeric(), y = numeric() , angle = numeric(), step = numeric(),  group = factor())
for(j in 1:10){ 
  temp <- tibble(x = 0, y = 0, angle = 0, step = 0, group = factor(j))
  group <- j
  for(i in 1:100){
    angle <- theta_runif()
    x <-  temp$x[i]+(cos(angle))
    y <-  temp$y[i]+(sin(angle))
    step <- cos(angle)**2 + sin(angle)**2
    group <- group
    temp <- rbind(temp, c(x, y, angle, step, group))
  }
  rw_control_points <- rbind(rw_control_points, temp)
  rm(temp, group)
}

ggplot(data = rw_control_points)+
  geom_path(aes(x, y, color = group))+
  coord_fixed()
summary(rw_control_points)



#random walk with variable step length

rw_control_points <- tibble(x = numeric(), y = numeric(), x_angle = numeric(), y_angle = numeric(), step = numeric(),
                            indx = numeric(), group = factor())
for(j in 1:10){ 
  temp <- tibble(x = 0, y = 0, x_angle = 0, y_angle = 0, step = 0, indx = 1, group = factor(j))
  group = j
  for(i in 1:10){
    x_angle <- theta_runif()
    y_angle <- theta_runif()
    x <-  temp$x[i]+(sin(x_angle))
    y <-  temp$y[i]+(cos(y_angle))
    step <- sin(x_angle)**2 + cos(y_angle)**2
    indx = temp$indx[i]+1
    group <- group
    temp <- rbind(temp, c(x, y, x_angle, y_angle, step,indx, group))
  }
  rw_control_points <- rbind(rw_control_points, temp)
  rm(temp, group)
}

ggplot(data = rw_control_points)+
  geom_path(aes(x, y, color = group, size = sqrt(indx)), lineend = "round")+
  coord_fixed()+
  scale_radius()
summary(rw_control_points)
?scale_radius

#random walk with small angular deviations in each step (it creates biased data, as seen on large walks)

theta_runif <- function(n = 1, min = 0, max = 2*pi, ...){
  (runif(n, min, max))
}

theta_rnorm <- function(mean = pi, sd = pi/2, ...){
  (rnorm(1, mean = mean, sd = sd))
}


rw_control_points <- tibble(x = numeric(), y = numeric() , angle_with_x = numeric(),  group = factor())

for(j in 1:100){ 
  temp <- tibble(x = c(0,runif(1, -1, 1)), y = c(0, runif(1, -1, 1)), group = c(factor(j), factor(j)),
                 angle_with_x = c(0, atan(x[2]-x[1] / y[2]-y[1])))
  group <- j
  for(i in 1:100){
    new_angle_with_x <- temp$angle_with_x[i+1] - theta_runif(1, max= pi/6)
    x <-  temp$x[i+1]+(sin(new_angle_with_x))
    y <-  temp$y[i+1]+(cos(new_angle_with_x))
    angle_with_x <- atan(x-temp$x[i] / y-temp$y[i+1])
    group <- group
    temp <- rbind(temp, c(x, y, group, angle_with_x))
    rm(x, y, angle_with_x)
  }
  rw_control_points <- rbind(rw_control_points, temp)
  rm(temp, group)
}

ggplot(data = rw_control_points)+
  geom_path(aes(x, y, group = group))+
  coord_fixed()
summary(rw_control_points)


#random walk with small angular deviations (points selected using coordinates, not trig functions)

theta_runif <- function(n = 1, min = 0, max = 2*pi, ...){
  (runif(n, min, max))
}

theta_rnorm <- function(mean = pi, sd = pi/2, ...){
  (rnorm(1, mean = mean, sd = sd))
}


rw_control_points <- tibble(x = numeric(), y = numeric() , angle_with_x = numeric(),  group = factor())

for(j in 1:20){ 
  temp <- tibble(x = c(0,runif(1, -1, 1)), y = c(0, runif(1, -1, 1)), group = c(factor(j), factor(j)),
                 angle_with_x = c(0, atan(x[2]-x[1] / y[2]-y[1])))
  group <- j
  for(i in 1:10){
    new_angle_with_x <- temp$angle_with_x[i+1] - theta_runif(1, max= pi/6)
    x <-  temp$x [i+1] + runif(1, -1, 1)
    y <-  temp$y[i+1] + rnorm(1, 0, (abs(temp$x[i+1]))**1/4)
    angle_with_x <- atan(x-temp$x[i] / y-temp$y[i+1])
    group <- group
    temp <- rbind(temp, c(x, y, group, angle_with_x))
    rm(x, y, angle_with_x)
  }
  rw_control_points <- rbind(rw_control_points, temp)
  rm(temp, group)
}

ggplot(data = rw_control_points)+
  geom_path(aes(x, y, group = group))+
  coord_fixed()

ggplot(data = rw_control_points)+
  geom_bspline(aes(x, y, group = group))
summary(rw_control_points)




# circles on random walks -------------------------------------------------


rw_circles <- tibble(x = numeric(), y = numeric(), r = numeric(), group = factor())

for(j in 1:10){ 
  temp <- tibble(x = c(0,runif(1, -1, 1)), y = c(0, runif(1, -1, 1)), r = c(x[1]/4, x[2])/4, group = c(factor(j), factor(j)))
  group <- j
  for(i in 1:10){
    x <-  temp$y[i+1]  + rnorm(1, 0,  abs(temp$x[i+1])/2) #+ (temp$x[i+1]/10)
    y <-  temp$x[i+1] + rnorm(1, 0, abs(temp$y[i+1])/2) #+ (temp$y[i+1]/10)
    r <- (abs(x) + abs(y)) / 10 # + runif(1, x/10, x/10)
    group <- group
    temp <- rbind(temp, c(x, y, r, group))
    rm(x, y, r)
  }
  rw_circles <- rbind(rw_circles, temp)
  rm(temp, group)
}

ggplot(data = rw_circles)+
  geom_path(aes(x, y, group = group))+
  coord_fixed()

ggplot(data = rw_circles)+
  geom_circle(aes(x0 = y, y0 =x, r = r, fill = group, linetype = NA), alpha = 0.7, show.legend = F)+
  geom_circle(aes(x0 = y, y0 =x, r = r*rnorm(120), fill = group), alpha = 0.7, show.legend = F)+
  coord_fixed()+
  scale_fill_manual(values = fill_colors_rw_circles)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank())



fill_colors_rw_circles <- c("a"="grey40",
                            "b"="grey50",
                            "c"="grey60",
                            "d"="grey70",
                            "e"="grey80",
                            "f"="grey90",
                            "g" = "white",
                            "h" = "black",
                            "1"= "#40c0ff", "2"="#E69F00", "3" = "#56B4E9",
                            "4"= "#009E73","5" = "#F0E442","6" = "#0072B2", 
                            "7" ="#D55E00","8"= "#CC79A7", "9" = "#ffc066", 
                            "10" = "550022", "11"= '440044')



# random circles on random spines ------------------------------------------------


random_points2 <- tibble(x = seq(0, 1, length.out = 5), 
                         y = runif(5, -1, 1), 
                         group = factor(1))

x1 <- splinefun(random_points1$x, random_points1$y)

x2 <- splinefun(random_points2$x, random_points2$y)

plot_points2 <- tibble(x = seq(from = 0, to = 1, length.out = 20), 
                       y = x2(x), 
                       r = y/2,
                       group = factor(2))
plot_points <- rbind(plot_points, plot_points2)


plot_points %>% 
  mutate(r = r/4, y = y/2) %>% 
  ggplot()+
  geom_circle(aes(x0=x, y0=y, r = r, fill = group, linetype = NA), alpha = 1/3)+
  coord_fixed()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank())
summary(plot)

x <- function(x) c(x1(x), x)
f <- function(t) c(sin(2*t), cos(t), t)

#### using "pointsOnBezier" from "bezier" package

random_points1 <- tibble(x = seq(0, 1, length.out = 10),
                         y = runif(10, -1, 1))                        
m_random_points1 <- as.matrix(random_points1)

b_random_points1 <- bezier(t = seq(0, 1, length = 100), p = m_random_points1)

pob1 <- pointsOnBezier(p = m_random_points1, n = 20, method = "evenly_spaced", print.progress = T)

pob_tb1 <- tibble(x = pob1$points[,1],
                  y = pob1$points[,2], 
                  r = y/4, 
                  group = factor(1))



## generating a second set of points

random_points2 <- tibble(x = seq(0, 1, length.out = 10),
                         y = runif(10, -1, 1))                        
m_random_points2 <- as.matrix(random_points2)

b_random_points2 <- bezier(t = seq(0, 1, length = 100), p = m_random_points2)

pob2 <- pointsOnBezier(p = m_random_points2, n = 20, method = "evenly_spaced", print.progress = T)

pob_tb2 <- tibble(x = pob2$points[,1] + rep(runif(1, 0, 0.2), 20),
                  y = pob2$points[,2], 
                  r = y/4, 
                  group = factor(2))

## third set
random_points3 <- tibble(x = seq(0, 1, length.out = 10),
                         y = runif(10, -1, 1))                        
m_random_points3 <- as.matrix(random_points3)

b_random_points3 <- bezier(t = seq(0, 1, length = 100), p = m_random_points3)

pob3 <- pointsOnBezier(p = m_random_points3, n = 20, method = "evenly_spaced",sub.relative.min.slope = 0.01, print.progress = T)

pob_tb3 <- tibble(x = pob3$points[,1] - rep(runif(1, 0, 0.2), 20),
                  y = pob3$points[,2], 
                  r = y/4, 
                  group = factor(3))

points_tb <- rbind(pob_tb1, pob_tb2, pob_tb3)

ggplot(data = points_tb)+
  geom_circle(aes(x0 = x, y0 = y, r = r, fill = group, linetype = NA), alpha = 1)+
  coord_fixed()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank())+
  scale_fill_viridis_d()
?pointsOnBezier

circles_on_splines <- function(ncircles= 20, nsplines = 10, ncontrol_points = 5, 
                               minx = 0, maxx = 1, miny = -0.5, maxy = 0.5, ...){
  pob_tb <- tibble(x = numeric(), y = numeric(), r = numeric(), group = factor())
  for(i in 1:nsplines){
    random_points <- tibble(x = c(0, runif(1, minx/3, maxx/3), seq(minx, maxx-0.3, length.out = ncontrol_points), maxx-0.2, maxx),
                            y = c(0, runif(1, miny/3, maxy/3), runif(ncontrol_points, miny, maxy), runif(1, miny/3, maxy/3), 0))
    random_points_matrix <- as.matrix(random_points)
    pob <- pointsOnBezier(p = random_points_matrix, n = ncircles, method = "evenly_spaced", sub.relative.min.slope = 0.1, print.progress = F)
    temp_pob_tb <- tibble(x = pob$points[,1],
                          y = pob$points[,2], 
                          r = (y/8) + sign(y)*0.10, 
                          group = factor(i))
    pob_tb <- rbind(pob_tb, temp_pob_tb)
    percent <- round((i/nsplines)*100)
    writeLines(paste(percent, "...", "%", sep = ""))
    rm(random_points, random_points_matrix, pob, temp_pob_tb)
  }
  ggplot(data = pob_tb)+
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = group, linetype = NA), alpha = 1/4, show.legend = F)+
    coord_fixed()+
    #geom_path(aes(x, y, color = group))+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}
circles_on_splines(ncircles = 30, nsplines = 5, ncontrol_points = 20, maxx = 5, miny = -4, maxy = 4)

# eliptical fields --------------------------------------------------------

?sample

# Lets make some data
arcs <- data.frame(
  start = seq(0, 2 * pi, length.out = 11)[-11],
  end = seq(0, 2 * pi, length.out = 11)[-1],
  r = rep(1:2, 5)
)

# Behold the arcs
ggplot(arcs) +
  geom_arc(aes(x0 = 0, y0 = 0, r = r, start = start, end = end,
               linetype = factor(r)))

ggplot(arcs) +
  geom_arc(aes(x0 = 0, y0 = 0, r = r, start = start, end = end,
               size = stat(index)), lineend = 'round') +
  scale_radius() # linear size scale

arcs2 <- data.frame(
  angle = c(arcs$start, arcs$end),
  r = rep(arcs$r, 2),
  group = rep(1:10, 2),
  colour = sample(letters[1:5], 20, TRUE)
)



elipses <- tibble(x = rep(0, 10), 
                  y = 1:10, 
                  a = rep(3, 10), 
                  b = rep(5, 10),
                  angle = pi/4)
ggplot(data = elipses)+
  geom_ellipse(aes(x0 = x, y0 = y, a = 2, b = b, angle = 0))+
  coord_fixed()

curve_ends <- tibble(id = 1:10,
                     x = rev(-10:-1), 
                     y = 0,
                     xend = 11:20, 
                     yend = 0,
                     group = factor(1:10)
)

ggplot(data = curve_ends)+
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, group = group), ncp = 10, curvature = 2)+
  coord_fixed(xlim = c(-15, 30), ylim = c(-50, 0.025))


?geom_curve
spline_right <- tibble(id = 1:10, 
                       x = seq(1, 3, length.out = 10), 
                       y = 0)

spline_left <- spline_right %>% 
  mutate(x = -x)

spline_mid <- spline_right %>% 
  mutate(y = 1:10, x = 0)

sp_cp <- rbind(spline_left, spline_mid, spline_right) %>% 
  arrange(id) %>% 
  mutate(group = rep(1:10, each = 3))


ggplot(data = sp_cp)+
  geom_bspline(aes(x, y, color = stat(abs(x)), group = group, size = stat(y)), lineend = "round", n = 1000)+
  coord_fixed()+
  scale_color_viridis_b()


# random lines ------------------------------------------------------------


?geom_segment

x = runif(1) 
y = runif(1) 
xend = runif(1) 
yend = runif(1) 
group = factor(1)
segment_ends <- tibble(x = x, 
                       y = y, 
                       xend = xend, 
                       yend = yend, 
                       group = group)

for(i in 1:110){
  tmp <- tibble(x = segment_ends$x[i]-0.5, 
                y = segment_ends$y[i]+0.2, 
                xend = segment_ends$xend[i]+ 0.1, 
                yend = segment_ends$yend[i]+0.1, 
                group = factor(i+1))
  segment_ends <- rbind(segment_ends, tmp)
}

segment_ends <- segment_ends %>% 
  mutate(id = 1:nrow(segment_ends))
segment_ends2 <- segment_ends %>% 
  mutate(x = -x,
         xend = -xend, 
         id = 1:nrow(segment_ends))

segment_merged <- rbind(segment_ends, segment_ends2) %>% 
  arrange(id)

ggplot(mapping = aes(size = 1))+
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = abs(x), alpha = y), data = segment_ends,
             curvature = -0.7, lineend = "round", show.legend = F)+
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = abs(x), alpha = y), data = segment_ends2,
             curvature = 0.7, lineend = "round", show.legend = F)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank())

?rgb

