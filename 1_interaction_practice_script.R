library(tidyverse)
library(apaTables)
library(MBESS)

analytic.data <- read_csv("mmr_practice_data.csv")

apa.cor.table(analytic.data, table.number = 1) #HOW TO WRITE UP CORRELATIONS

## DO ANXIETY & PREPARATION INTERACTION TO PREDICT EXAM SCORES?

# only keep columns we need...
analytic.data <- analytic.data %>% select(exam, anxiety, preparation)

# keep complete cases only...
analytic.data <- na.omit(analytic.data)

# centre variables...
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(anxiety, center=T, scale=F)))
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(preparation, center=T, scale=F)))

# compute regression including interaction...
interaction.regression <- lm(exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)

# get values for the text this way...
summary(interaction.regression)

# regression table...
apa.reg.table(interaction.regression, table.number=2)
##  WRITE-UP!!!

# OR, can use the block apprach...
block1 <- lm(exam ~ x.centered + z.centered, data = analytic.data, na.action = na.exclude)
block2 <- lm(exam ~ x.centered + z.centered + I(x.centered*z.centered), data = analytic.data, na.action = na.exclude)
apa.reg.table(block1, block2)
##  Look at delta R2 - will give same value as the reg table approach
##  WRITE-UP!!!


# MAKE GRAPH - GETTING THE LINES ON THE SURFACE (+1 SD)

# get sd of z, then mutate a bit...
sd.z <- sd(analytic.data$z.centered, na.rm = TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
##  This may seem counter-intuitive, but we lower the scores to increase the zero point to +1 SD

# get formula...
simple.slope.plus.1SD <- lm(exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD), data = analytic.data, na.action=na.exclude)
summary(simple.slope.plus.1SD) #values for text this way!

# make regression table...
apa.reg.table(simple.slope.plus.1SD) #drop last 2 lines - look only at x.centered and intercept lines


# MAKE GRAPH - GETTING THE LINES ON THE SURFACE (-1 SD)

# get sd of z, then mutate a bit...
sd.z <- sd(analytic.data$z.centered, na.rm = TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD = z.centered - sd.z)
##  This may seem counter-intuitive, but we increase the scores to decrease the zero point to -1 SD

# get formula...
simple.slope.minus.1SD <- lm(exam ~ x.centered + z.centered.at.minus.1SD + I(x.centered*z.centered.at.minus.1SD), data = analytic.data, na.action=na.exclude)
summary(simple.slope.minus.1SD) #values for text this way!

# make regression table...
apa.reg.table(simple.slope.minus.1SD) #drop last 2 lines - look only at x.centered and intercept lines


# FIGURE 2: 2D PLOT

# 2D Graph range on X-axis...
sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

x.axis.range <- seq(-1*sd.x, 1*sd.x, by=.25*sd.x) #this indicates range on the x-axis of -2SD to +2SD Anxiety

# 2D Graph Lines for +1 SD and -1 SD on Prep...
sd.z <- sd(analytic.data$z.centered, na.rm = TRUE)
z.line.hi = 1*sd.z
z.line.lo = -1*sd.z

# 2D Graph: Create data for drawing lines...
## +1 SD line:
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.hi)

## -1 SD line:
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.lo)

# put the info describing the lines into a data frame...
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

# make the graph using the above data...
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

# make +1 SD Z line (b/c it is the one in the aes statement above)...
my.plot <- my.plot + geom_line(color="black", linetype="dotted", size=1.5)

# make -1 SD Z line...
my.plot <- my.plot + geom_line(aes(x=x.axis.range, y=y.values.at.minus.1SD.z), color="black", linetype="solid", size=1.5)

# set APA part of graph below...
my.plot <- my.plot + theme_classic(18)

# labels...
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")
my.plot <- my.plot + annotate("text", x = -1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot + annotate("text", x = -1, y = 43.5, label = "-1 SD Preparation")

# the SD of Anxiety (see Table 1 is 2.00 so -1SD to +1SD is -2 to 2)...
my.plot <- my.plot + coord_cartesian(xlim=c(-1, 1), ylim=c(0,100))

print(my.plot)
# save using menu's in RStudio. Manually include in MSword doc


# FIGURE 1: 3D PLOT

# see: summary(interaction.regression) for numbers to input here...
sd.x <- sd(analytic.data$x.centered, na.rm = TRUE)
intr.plot(b.0=47.04877, b.x=15.011113, b.z=9.45285, b.xz=22.60574, x.min=-1*sd.x, x.max=1*sd.x, z.min=-1*sd.z, z.max = 1*sd.z, xlab="Anxiety Centered", zlab = "Preparation Centered", ylab = "Exam Score", expand=1, hor.angle = 60, gray.scale = TRUE, line.wd = 4, zlim=c(0,100))
# save using menu's in RStudio. Manually include in MSword doc

### TO DO:
# Refresh on interpreting regression tables
# Finish write-ups