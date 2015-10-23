library(raster)
library(ggplot2)
library(plyr)

# Load the data
rgb_file <- '/Users/jduckles/Dropbox/NEON_DataCarp_Hackathon/Data/1_WorkshopData/NEON_RemoteSensing/SJER/SJER_camera.tif'

# Read each band from the file individually
red <- raster(rgb_file,band=1)
green <- raster(rgb_file,band=2)
blue <- raster(rgb_file,band=3)

# Generate a gray scale color map
colors = gray(seq(0,1,0.01))

# Plot each of the layers
plot(red,col=colors)
plot(green,col=colors)
plot(blue,col=colors)

# Generate a raster stack with all three bands
rgb_stack <- stack(c(red,green,blue))

# Use GUI to pick points
pick_points <- function() {
  pxy <- locator(4) # Select Location(s)
  locations <- cbind(pxy$x,pxy$y) #Make into df
  locations # function returns locations 
}

points <- pick_points() 

wavelengths <- c(675,550,450) # nm bands centers for RGB camera

# Extract R, G, B Digital Numbers from three bands at the four clicked locations
spectral <- extract(rgb_stack, points) 

# Plot RGB image as background
plotRGB(rgb_stack,r=1,g=2,b=3)
points(points, col=pix_colors, pch=17, cex=3)

# Build a data frame from 
specs <- adply(spec, 1, function(dn) { cbind(wavelengths,dn) })


pix_colors <- c("indianred4", "blue", "darkslategray4", "mediumorchid1")

plotRGB(rgb_stack,r=1,g=2,b=3)
ggplot(specs, aes(x=wavelengths,y=dn, group=X1, color=X1)) + geom_line() + geom_point()


setwd('/Users/jduckles/Dropbox/NEON_DataCarp_Hackathon/Data/Landsat/')
#bring in landsat bands and stack
b2 <- raster("LC80420342015210LGN00_SJR_sub_b2.tif")
b3 <- raster("LC80420342015210LGN00_SJR_sub_b3.tif")
b4 <- raster("LC80420342015210LGN00_SJR_sub_b4.tif")
b5 <- raster("LC80420342015210LGN00_SJR_sub_b5.tif")
b6 <- raster("LC80420342015210LGN00_SJR_sub_b6.tif")
b7 <- raster("LC80420342015210LGN00_SJR_sub_b7.tif")

#stack and display
ms_stack < -stack(b2,b3,b4,b5,b6,b7)
ms_image_scale <- max(maxValue(ms_stack))

#display image with linear stretch
plotRGB(ms_stack, r=4, g=3, b=2, scale=ms_image_scale, stretch="lin")

zoom <- extent(c(256737.7, 4110509, 257349.0, 4110021))

landsat_extent <- extent(ms_stack)
red_cropped <- crop(red, zoom)
green_cropped <- crop(green, zoom)
?crop
plot(red_cropped)
plotRGB(rgb_cropped, r=1, g=b, b=2)


