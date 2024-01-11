# Container Ship
# an idea by https://twitter.com/philjeanpierre/status/1442122095715143682
library(ggplot2)
library(ggside) # lets us customize side panels on plots
library(wesanderson) # groovy colors

# Dummy data
x <- as.factor(LETTERS[1:20])
y <- paste0("var", seq(1,10))
data <- expand.grid(x=x, y=y)
data$z <- runif(200, 0, 5)

pal <- wes_palette("Zissou1", 10, type = "continuous")
ggplot(data, aes(x,y, fill= z)) + 
  # make the overhead view of ship
  geom_tile(color = "black") + 
  scale_fill_gradientn(colours = pal) + 
  # now make the top side panel, the shadows.
  geom_xsidecol(aes(x=x,y=z),color="black",fill="black") + 
  theme_void() + 
  theme(legend.position = "none") + 
  theme(plot.background = element_rect(fill = "darkgray"),
        panel.background = element_rect(fill = "darkgray")) +
# play with these numbers to change the relative size and aspect of each panel.
  theme(ggside.panel.scale.x = 1.8) + 
  coord_fixed(ratio = .1)
