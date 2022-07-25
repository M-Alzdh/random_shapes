### Includes a bunch of scripts to make random shapes using ggplot
### you should load in the following packages: "tidyverse", "ggforce", "colortools", and "bezier" 

library(tidyverse)
library(ggforce)
library(bezier)


### this function is used throughout to create n random colors, to set 'alpha = T" for variable opacity

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

### these two functions are used in random walks to generate the next step

theta_runif <- function(n = 1, min = 0, max = 2*pi, ...){
  (runif(n, min, max))
}

theta_rnorm <- function(mean = pi, sd = pi/2, ...){
  (rnorm(1, mean = mean, sd = sd))
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

### these fucntions simulate random walks and draw paths or splines on them
### some depend on the two following functions to pick the next step
### find an efficient way to randominze colors

theta_runif <- function(n = 1, min = 0, max = 2*pi, ...){
  (runif(n, min, max))
}

theta_rnorm <- function(mean = pi, sd = pi/2, ...){
  (rnorm(1, mean = mean, sd = sd))
}



#random walk keeping step length constant

rnd_walk_constant_step <- function(n_walks = 10, n_steps = 10){
  rw_control_points <- tibble(x = numeric(), y = numeric() , angle = numeric(),
                              step = numeric(),  group = factor())
  for(j in 1:n_walks){ 
    temp <- tibble(x = 0, y = 0, angle = 0, step = 0, group = factor(j))
    group <- j
    for(i in 1:n_steps){
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
    geom_path(aes(x, y, color = group), show.legend = F)+
    coord_fixed()+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}


#test the function
rnd_walk_constant_step(n_steps = 100, n_walks = 10)


#random walk with variable step length

### this function does not keep step length constant
### also, the line gets thicker as it grows

rnd_walk_var_step <- function(n_walks = 10, n_steps = 10){
  rw_control_points <- tibble(x = numeric(), y = numeric(), x_angle = numeric(), y_angle = numeric(), step = numeric(),
                              indx = numeric(), group = factor())
  for(j in 1:n_walks){ 
    temp <- tibble(x = 0, y = 0, x_angle = 0, y_angle = 0, step = 0, indx = 1, group = factor(j))
    group = j
    for(i in 1:n_steps){
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
    geom_path(aes(x, y, color = group, size = sqrt(indx)), lineend = "round", show.legend = F, alpha = 0.7)+
    coord_fixed()+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}

#test the function
rnd_walk_var_step(n_walks = 5, n_steps = 20)
  

#random walk with small angular deviations in each step (it creates biased data, as seen on long walks)


"rw_control_points <- tibble(x = numeric(), y = numeric() , angle_with_x = numeric(),  group = factor())

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
summary(rw_control_points)"


#random walk with small angular deviations (points selected using coordinates, not trig functions)
### can do either splines or paths (use' type = "spline"' for splines)

rnd_walk_small_dev <- function(n_walks = 10, n_steps = 10, type = "path"){
  rw_control_points <- tibble(x = numeric(), y = numeric() , group = factor())
  for(j in 1:n_walks){ 
    temp <- tibble(x = c(0,runif(1, -1, 1)), y = c(0, runif(1, -1, 1)), group = c(factor(j), factor(j)))
    group <- j
    for(i in 1:n_steps){
      x <-  temp$x [i+1] + runif(1, -1, 1)
      y <-  temp$y[i+1] + rnorm(1, 0, (abs(temp$x[i+1]))**1/4)
      group <- group
      temp <- rbind(temp, c(x, y, group))
      rm(x, y)
    }
    rw_control_points <- rbind(rw_control_points, temp)
    rm(temp, group)
  }
  if(type == "spline"){
    ggplot(data = rw_control_points)+
      geom_bspline(aes(x, y, color = group), show.legend = F)+
      coord_fixed()+
      theme(panel.background = element_blank(), 
            panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_blank())
  } else {
    ggplot(data = rw_control_points)+
      geom_path(aes(x, y, color = group), show.legend = F)+
      coord_fixed()+
      theme(panel.background = element_blank(), 
            panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_blank())
  }
}

#test the function
rnd_walk_small_dev(n_walks = 15, n_steps = 20, type = "spline")


# circles on random walks -------------------------------------------------

### this function first creates random points (similar to nodes on a random walk)
### and then draws circles on those

rnd_circles_rnd_walks <- function(n_walks = 10, n_circles = 10){
  rw_circles <- tibble(x = numeric(), y = numeric(), r = numeric(), group = factor())
  for(j in 1:n_walks){ 
    temp <- tibble(x = c(0,runif(1, -1, 1)), y = c(0, runif(1, -1, 1)), r = c(x[1]/4, x[2])/4, group = c(factor(j), factor(j)))
    group <- j
    for(i in 1:n_circles){
      
      # here, x and y are generated y and x in the previous step, plus/minus some random value
      # changing how x and y are generated can greatly change how the final graph looks like
      # the size of the circles is a function of their distance from [0,0]
      
      x <-  temp$y[i+1]  + runif(1, -abs(temp$x[i+1]),  abs(temp$x[i+1])) #+ (temp$x[i+1]/10)
      y <-  temp$x[i+1] + runif(1, -abs(temp$y[i+1]), abs(temp$y[i+1])) #+ (temp$y[i+1]/10)
      r <- sqrt(abs(x) + abs(y))/6 # + runif(1, x/10, x/10)
      group <- group
      temp <- rbind(temp, c(x, y, r, group))
      rm(x, y, r)
    }
    rw_circles <- rbind(rw_circles, temp)
    rm(temp, group)
  }

  ggplot(data = rw_circles)+
    geom_circle(aes(x0 = y, y0 =x, r = r, fill = group, linetype = NA), alpha = 0.7, show.legend = F)+
    # r is used to randomly make the circle with the line around it larger or smaller
    geom_circle(aes(x0 = y, y0 =x, r = r*rnorm(n_walks*(n_circles+2), 1, 1), fill = group), alpha = 0.7, show.legend = F)+
    coord_fixed()+
    scale_fill_manual(values = rnd_colors(n_walks))+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())


}

#test the function
rnd_circles_rnd_walks(n_walks = 10, n_circles = 10)




# random circles on random spines ------------------------------------------------

### this function first selcts a number of control points (n_control_points), then fits a 
### bezier curve to them, then draws n_circles equally spaced circles on the curve 
### the function repeats this nsplines times
### computing equally spaced points on a curve takes a while

rnd_circles_on_splines <- function(n_circles= 20, n_splines = 10, n_control_points = 5, 
                               min_x = 0, max_x = 1, min_y = -0.5, max_y = 0.5, ...){
  pob_tb <- tibble(x = numeric(), y = numeric(), r = numeric(), group = factor())
  for(i in 1:n_splines){
    random_points <- tibble(x = c(0, runif(1, min_x/3, max_x/3), seq(min_x, max_x-0.3, length.out = n_control_points), max_x-0.2, max_x),
                            y = c(0, runif(1, min_y/3, max_y/3), runif(n_control_points, min_y, max_y), runif(1, min_y/3, max_y/3), 0))
    random_points_matrix <- as.matrix(random_points)
    pob <- pointsOnBezier(p = random_points_matrix, n = n_circles, method = "evenly_spaced", sub.relative.min.slope = 0.1, print.progress = F)
    temp_pob_tb <- tibble(x = pob$points[,1],
                          y = pob$points[,2], 
                          r = (y/8) + sign(y)*0.10, 
                          group = factor(i))
    pob_tb <- rbind(pob_tb, temp_pob_tb)
    percent <- round((i/n_splines)*100)
    writeLines(paste(percent, "...", "%", sep = ""))
    rm(random_points, random_points_matrix, pob, temp_pob_tb)
  }
  ggplot(data = pob_tb)+
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = group, linetype = NA), alpha = 1/4, show.legend = F)+
    # r is used to randomly make the circle with the line around it larger or smaller
    geom_circle(aes(x0 = x, y0 = y, r = r*runif(nrow(pob_tb)), fill = group, linetype = NA), alpha = 1/4, show.legend = F)+
    coord_fixed()+
    scale_fill_manual(values = rnd_colors(n_splines, alpha = T))+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
}
rnd_circles_on_splines(n_circles = 10, n_splines = 5, n_control_points = 5, max_x = 5, min_y = -4, max_y = 4)









##### ANYTHING AFTER THIS POINT IS A WORK IN PROGRESS

# eliptical fields --------------------------------------------------------

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


# iris --------------------------------------------------------------------



?geom_segment
### generate random x and y

x <- runif(50, -1, 1)
y <- runif(50, -1, 1)

which_quad <- function(x, y){
 case_when(
   x > 0 & y > 0 ~ 1,
   x <= 0 & y > 0 ~ 2,
   x <= 0 & y <= 0 ~ 3,
   x > 0 & y <= 0 ~ 4
 ) 
}




tb <- tibble(x = x, y = y, quad = which_quad(x, y), theta = atan(x/y),
             x2 = case_when(
               quad == 1 ~ sin(theta), 
               quad == 2 ~ sin(theta), 
               quad == 3 ~ -sin(theta), 
               quad == 4 ~ -sin(theta)
             ),
            y2 = case_when(
                quad == 1 ~ cos(theta), 
                quad == 2 ~ cos(theta), 
                quad == 3 ~ -cos(theta), 
                quad == 4 ~ -cos(theta)
              ), 
            theta2 = theta + theta_runif(50, min = -pi/6, max = pi/6),
            x3 = case_when(
              quad == 1 ~ sin(theta2) + runif(1), 
              quad == 2 ~ sin(theta2) - runif(1), 
              quad == 3 ~ -sin(theta2) - runif(1),
              quad == 4 ~ -sin(theta2) + runif(1)
            ),
            y3 = case_when(
              quad == 1 ~ cos(theta2) + runif(1), 
              quad == 2 ~ cos(theta2) + runif(1), 
              quad == 3 ~ -cos(theta2) - runif(1), 
              quad == 4 ~ -cos(theta2) - runif(1)
            ), 
            )




ggplot(data = tb)+
  geom_point(aes(x3, y3, color = as.factor(quad)))+
  geom_point(aes(x2, y2, color = as.factor(quad)))+
  geom_segment(aes(x = x2, y = y2, xend = x3, yend = y3), arrow = arrow())+
  coord_fixed()

?geom_segment

plot(tb$theta)
-sin(0.367)


tb2 <- tibble(x = x, y = y, quad = which_quad(x, y), theta = atan(x/y),
             x2 = case_when(
               quad == 1 ~ sin(theta), 
               quad == 2 ~ sin(theta), 
               quad == 3 ~ -sin(theta), 
               quad == 4 ~ -sin(theta)
             ),
             y2 = case_when(
               quad == 1 ~ cos(theta), 
               quad == 2 ~ cos(theta), 
               quad == 3 ~ -cos(theta), 
               quad == 4 ~ -cos(theta)
             ),
             theta2 = atan2(x2, y2)
             )

ggplot(data = tb2)+
  geom_point(aes(x, y), color = "black")+
  geom_point(aes(x2, y2), color = "red")+
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2), arrow = arrow())+
  geom_text(aes(x = x2, y = y2, label = signif(theta2)))+
  coord_fixed()

tb3 <- tb2 %>% 
  mutate(theta3 = theta2 + theta_rnorm(mean = theta2, sd = pi/6), 
         x3 = 2*cos(theta3), 
         y3 = 2*sin(theta3)
         )
ggplot(data = tb3)+
  geom_point(aes(x2, y2), color = "black")+
  geom_point(aes(x3, y3), color = "red")+
  geom_segment(aes(x = x2, y = y2, xend = x3, yend = y3), arrow = arrow())+
  #geom_text(aes(x = x2, y = y2, label = signif(theta3)))
  coord_fixed()

# whorl -------------------------------------------------------------------


tb <- tibble(angle = seq(0, 2*pi, length.out = 100), 
             x = cos(angle), y = sin(angle),
               angle2 = angle + pi/20, 
             d = runif(100, 1.8, 2),
             x2 = d*cos(angle2), y2 = d*sin(angle2))
ggplot(data = tb)+
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2))+
  coord_fixed()

# smoothing ---------------------------------------------------------------


  mtrx_smoother <- function(x, window_size = 3, smoother = mean){
    # making sure that the inputs are the correct type size
      ## x must be a matrix
    stopifnot(is.matrix(x))
      ## window must be an odd integer larger than 2
    stopifnot(!is.integer(window_size))
    stopifnot(window_size %% 2 != 0 | window_size > 2)
    
    #getting matrix info
    n_row <- nrow(x)
    n_col <- ncol(x)
    
    #smoothing
      ## create empty matrix of the same size and pad the edges with 0's
      mtrx <- x
      mtrx <- rbind(0, mtrx, 0)
      mtrx <- cbind(0, mtrx, 0)
      print(mtrx)
    
      ## perform moving window smoothing
      smoothed_mtrx <- mtrx
      
      for(i in 1:n_row + 1){
        for(j in 1:n_col + 1){
          smoothed_mtrx[i, j] <- mean(c(mtrx[i-1, j-1], mtrx[i-1, j], mtrx[i-1, j+1], 
                                        mtrx[i, j-1], mtrx[i, j], mtrx[i, j+1], 
                                        mtrx[i+1, j-1], mtrx[i+1, j], mtrx[i+1, j+1]))
        }
      }

    smoothed_mtrx <- smoothed_mtrx[-c(1, n_row+2), -c(1, n_col+2)]
    
   (out <- list(input_matrix = x, smoothed_matrix = smoothed_mtrx))
}



tst_mtrx <- matrix(ceiling(runif(16, 1, 10)), nrow = 4)

mtrx_smoother(tst_mtrx)

