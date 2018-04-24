# load in the reddit submissions 
library(readr)
reddit <- read_csv("E:/github/R_Programming/submissions.csv")
View(reddit)

# create a vector of unique subredits (no repeating subreddits)
# subreddits <- unique(reddit[8], incomparables = FALSE)
# View(subreddits)

# use to find the number of times a subreddit occurs
sub_posts <- data.frame(table(reddit$subreddit))
# 867 subreddits

post_num <- sub_posts$Freq

post_std_dev <- sd(post_num)
# 2160.7

library(fitdistrplus)
library(logspline)

descdist(post_num, discrete = FALSE)

# summary statistics
# ------
# min:  1   max:  55281 
# median:  2 
# mean:  152.6032 
# estimated sd:  2160.703 
# estimated skewness:  21.38811 
# estimated kurtosis:  514.6235 

fit.norm <- fitdist(post_num, "norm")
plot(fit.norm)

fit.weibull <- fitdist(post_num, "weibull")
plot(fit.weibull

fit.log <- fitdistr(post_num, "lognormal")

sub_posts_ordered <- sub_posts[ order(sub_posts[,2]), ]

#grab all subredits with posts greater than 10 and less than 100
mod_posts <- subset(sub_posts, Freq>50 & Freq<100)

descdist(mod_posts$Freq, discrete = FALSE)
# summary statistics
# ------
# min:  52   max:  95 
# median:  75 
# mean:  75.27273 
# estimated sd:  12.13335 
# estimated skewness:  -0.2361813 
# estimated kurtosis:  3.050051 

mod_fit_norm = fitdist(mod_posts$Freq, "norm")
plot(mod_fit_norm)

mod_post_1_50 <- subset(sub_posts, Freq>=1 & Freq<50)

descdist(mod_post_1_50$Freq, discrete = FALSE)
# summary statistics
# ------
# min:  1   max:  49 
# median:  2 
# mean:  4.320574 
# estimated sd:  6.258129 
# estimated skewness:  3.037712 
# estimated kurtosis:  14.5649

mod_1_50_fit <- fitdist(mod_post_1_50$Freq , "norm")


# mod 50 to 100 std div graph
pop_mean <- 75.3
pop_sd <- 12.13
sd_fill <- 1
lower_bound <- pop_mean - pop_sd * sd_fill
upper_bound <- pop_mean + pop_sd * sd_fill

x <- seq(-4, 4, length=100) * pop_sd + pop_mean

y <- dnorm(x, pop_mean, pop_sd)

plot(x, y, type="n", xlab="Post Number", ylab="",  main = "Distribution of Subreddit Postd 50-100", axes = FALSE)
lines(x, y)

bounds_filter <- x >= lower_bound & x <= upper_bound
x_within_bounds <- x[bounds_filter]
y_within_bounds <- y[bounds_filter]
 
# We want the filled in area to extend all the way down to the y axis which is why these two lines are necessary
# It makes the first point in the polygon (lower_bound, 0) and the last point (upper_bound, 0)
x_polygon <- c(lower_bound, x_within_bounds, upper_bound)
y_polygon <- c(0, y_within_bounds, 0)
 
polygon(x_polygon, y_polygon, col = "red")
 
# Now determine the probability that someone falls between the two bounds so we can display it above the curve
# Remember that pnorm returns the probability that a normally distributed random number will be less than the given number
probability_within_bounds <- pnorm(upper_bound, pop_mean, pop_sd) - pnorm(lower_bound, pop_mean, pop_sd)
 
# Concatenate the various values so we can display it on the curve
text <- paste("p(", lower_bound, "< post number <", upper_bound, ") =", signif(probability_within_bounds, digits = 3))
 
# Display the text on the plot. The default "side" parameter is 3, representing the top of the plot.
mtext(text)
 
# Add an axis to the current plot, where:
# - side: which side of the plot the axis should be drawn on where 1 represents the bottom
# - at: the points at which the tick-marks are to be drawn
# - pos: the coordinate at which the axis line is to be drawn
sd_axis_bounds = 5
axis_bounds <- seq(-sd_axis_bounds * pop_sd + pop_mean, sd_axis_bounds * pop_sd + pop_mean, by = pop_sd)
axis(side = 1, at = axis_bounds, pos = 0)



#mod 1 to 50 std div graph
pop_mean <- 4.320574
pop_sd <- 6.258129
sd_fill <- 1
lower_bound <- pop_mean - pop_sd * sd_fill
upper_bound <- pop_mean + pop_sd * sd_fill

x <- seq(-4, 4, length=100) * pop_sd + pop_mean

y <- dnorm(x, pop_mean, pop_sd)

plot(x, y, type="n", xlab="Post Number", ylab="",  main = "Distribution of Subreddit Postd 1-50", axes = FALSE)
lines(x, y)

bounds_filter <- x >= lower_bound & x <= upper_bound
x_within_bounds <- x[bounds_filter]
y_within_bounds <- y[bounds_filter]
 
# We want the filled in area to extend all the way down to the y axis which is why these two lines are necessary
# It makes the first point in the polygon (lower_bound, 0) and the last point (upper_bound, 0)
x_polygon <- c(lower_bound, x_within_bounds, upper_bound)
y_polygon <- c(0, y_within_bounds, 0)
 
polygon(x_polygon, y_polygon, col = "blue")
 
# Now determine the probability that someone falls between the two bounds so we can display it above the curve
# Remember that pnorm returns the probability that a normally distributed random number will be less than the given number
probability_within_bounds <- pnorm(upper_bound, pop_mean, pop_sd) - pnorm(lower_bound, pop_mean, pop_sd)
 
# Concatenate the various values so we can display it on the curve
text <- paste("p(", lower_bound, "< post number <", upper_bound, ") =", signif(probability_within_bounds, digits = 3))
 
# Display the text on the plot. The default "side" parameter is 3, representing the top of the plot.
mtext(text)
 
# Add an axis to the current plot, where:
# - side: which side of the plot the axis should be drawn on where 1 represents the bottom
# - at: the points at which the tick-marks are to be drawn
# - pos: the coordinate at which the axis line is to be drawn
sd_axis_bounds = 5
axis_bounds <- seq(-sd_axis_bounds * pop_sd + pop_mean, sd_axis_bounds * pop_sd + pop_mean, by = pop_sd)
axis(side = 1, at = axis_bounds, pos = 0)
