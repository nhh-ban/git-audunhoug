# Assignment 5
# Problem 2

setwd("/Users/audunhoug/desktop/BAN400/git-audunhoug/")

library(tidyverse)

# reading the text document and creating a data frame, skipping the first 12 lines
# and defining the deliminators
galaxy.df <- read_delim("suites_dw_Table1.txt",
                          skip = 12,
                          delim = "|")

# removing the empty first row 
galaxy.df <- galaxy.df[-1,]

# removing the whitespace in the column names
colnames(galaxy.df) <- trimws(colnames(galaxy.df))

# turning the appropriate columns into numeric values
galaxy.df <- galaxy.df %>%
  mutate_at(vars(a_26, m_b, log_lk, log_m26, log_mhi, vlg, ti1, D, delta_vlg),
            as.numeric)

# storing the first 12 lines in the document as a character
galaxy.desc <- read_lines("suites_dw_Table1.txt", n_max = 11)

galaxy.desc

# Problem 3 

# histogram showing the distribution of galaxy size and frequency in the data 
# using the absolute magnitude, which shows a somewhat skewed distribution
galaxy.df %>%
  ggplot(aes(x = m_b)) +
  geom_histogram()

# i tried looking at the the article for an explanation, and a point that 
# was brought up was that since the absolute magnitude of the brightness 
# vary tremendously, smaller and dimmer galaxies often end up drowning in the
# light of neighboring galaxies. to illustrate this with a plot i tried plotting
# the absolute magnitude against the distance to the galaxy. in the plot we see
# that the dimmer galaxies are all relatively close, while the galaxies with 
# an absolute magnitude of -10 or brighter (as its an inverse measurement)
# seem to be more evenly distributed. the explanation for this might be that 
# we have to be somewhat close to smaller galaxies in order to observe or detect
# them, and at a distance that might be more difficult.

galaxy.df %>%
  ggplot(aes(y = m_b, x = D)) +
  geom_point(alpha = 0.5)


# Problem 4

# creating the data frame and cleaning it up
hubble.df <- read_delim("UCNG_Table4.txt",
                        delim = "|")
hubble.df <- hubble.df[-1,]
colnames(hubble.df) <- trimws(colnames(hubble.df))
hubble.df <- hubble.df %>%
  mutate_at(vars(cz, error),
            as.numeric)

# creating the joint data frame 
joint.df <- full_join(x = galaxy.df, y = hubble.df, by = "name")

# plotting 
joint.df %>% 
  ggplot(aes(y = cz, x = D)) +
  geom_point(alpha = 0.5)

# in the plot we can see a proportionality between the the distance from us 
# and the velocity of the galaxies.

# estimating the Hubble constant using a linear model
model <- lm(cz ~ D, data = joint.df)
hubble_constant <- coef(model)["D"]

# storing the predicted values as a df to plot later
line_data <- data.frame(D = joint.df$D, v_predicted = hubble_constant * joint.df$D)

# plotting the predicted values in with the observations. the numeric value of H
# does not seem to fit with the values i found online, but it does seem to be a decent
# fit with our data.
joint.df %>% 
  ggplot(aes(y = cz, x = D)) +
  geom_point(alpha = 0.5) +
  geom_line(data = line_data, aes(y = v_predicted), color = "red", size = 1)






















