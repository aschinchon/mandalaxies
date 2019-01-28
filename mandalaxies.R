# Load packages
library(Rcpp)
library(tidyverse)

opt <-  theme(legend.position  = "none",
            panel.background = element_rect(fill="black", color="black"),
            plot.background  = element_rect(fill="black"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

# Cpp function to build the dataframe according the equations
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a1, double a2, double a3, double a4, double a5, 
            double a6) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = 10*a1+(x[i-1]+a2*sin(a3*y[i-1]+a4))*cos(2*M_PI/(13+10*a6))+y[i-1]*sin(2*M_PI/(13+10*a6));
            y[i] = 10*a5-(x[i-1]+a2*sin(a3*y[i-1]+a4))*sin(2*M_PI/(13+10*a6))+y[i-1]*cos(2*M_PI/(13+10*a6));
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

# Parameters (change them to create your own mandalaxies)
a1 <- -1.2
a2 <- 0.8
a3 <- -1.2
a4 <- -0.3
a5 <- -0.4
a6 <- 0.7

# Number of points to draw
npoints <- 4000000

# Creation of the data frame
df <- createTrajectory(npoints, 0, 0, a1, a2, a3, a4, a5, a6)  
  
# Plot
plot <- ggplot(df) +
      geom_point(aes(x, y), shape=46, alpha=0.01, size=0, color="white") +
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      coord_fixed() + opt
  
# Do you like it? Save it!
ggsave("choose_a_name.png", plot, height = 5, width = 5, units = 'in')
