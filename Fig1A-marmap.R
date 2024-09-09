#https://www.molecularecologist.com/2015/07/03/marmap/

library(marmap)
# 22.551418447824512, 105.70051840731072
# 13.916051823849621, 114.24788068813865

bluehole <- getNOAA.bathy(lon1 = 106.70051840731072, lon2 = 114.24788068813865,
                          lat1 = 22.551418447824512, lat2 = 13.916051823849621, 
                          res = 3, keep=TRUE)

pdf(file="YBH_map.pdf", width = 10, height = 12, pointsize = 18)

plot(bluehole, lwd = 0.2)
#plot(bluehole, n = 1, lwd = 0.7, add = TRUE)

scaleBathy(bluehole, deg = 2, x = "bottomleft", inset = 5)
loc <- data.frame(111.768056, 16.575) # loc of the YBH

buf <- create.buffer(bluehole, loc, radius=1.5)  # add a buffer circle

# Get the surface within the buffer for several depth slices
surf1 <- get.area(buf, level.inf=-100, level.sup=-1)
surf2 <- get.area(buf, level.inf=-500, level.sup=-100)
surf3 <- get.area(buf, level.inf=-1000, level.sup=-500)
surf4 <- get.area(buf, level.inf=-3000, level.sup=-1000)
s1 <- round(surf1$Square.Km)
s2 <- round(surf2$Square.Km)
s3 <- round(surf3$Square.Km)
s4 <- round(surf4$Square.Km)
# Add buffer elements on the plot
col.surf1 <- rgb(0.7, 0.7, 0.3, 0.3) #yellow
col.surf2 <- rgb(0, 0.7, 0.3, 0.3) #green
col.surf3 <- rgb(97/255, 156/255, 1, 0.3) #blue
col.surf4 <- rgb(0.7, 0, 0, 0.3) #red
plotArea(surf1, col = col.surf1)
plotArea(surf2, col = col.surf2)
plotArea(surf3, col = col.surf3)
plotArea(surf4, col = col.surf4)


plot(outline.buffer(buf$buffer), lwd = 1, add=TRUE)

points(loc, pch = 19, col = "red")

## Add legend
legend("topleft", fill = c(col.surf1, col.surf2, col.surf3, col.surf4),
       legend = c("0 ~ 100 m",
                  "100 ~ 500 m",
                  "500 ~ 1000 m",
                  "below 1000 m"))

dev.off()