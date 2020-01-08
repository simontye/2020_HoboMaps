###############################################################
# Hobo Maps
# 2020.01.03
# NTT, SPT
###############################################################
### Setup workspace

# Reset global enviroment
rm(list=ls())

# Reset loaded packages
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

# Install package
#install.packages("dplyr")

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rworldmap)
library(rworldxtra)
library(ggmap)
library(animation)
library(maps)
library(maptools)
library(htmlwidgets)
library(ggrepel)
library(cowplot)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmlwidgets)
library(tools)
library(extrafont)

###############################################################
### Load data

# Set working directory
setwd("/Users/simontye/Documents/Research/Projects/Hobo_Maps")

# Load files (data.#a files = 1885 - 1946; data.#b files = 1929 - 1939; data.#c files = 1885 - 1946 w/o #40 that went to Canada)
data.1a  <- read.csv(file = "Hobo_Trips.csv", head = TRUE, sep = ",")
data.1b  <- subset(data.1a, Year_Start >= 1929)
data.1b  <- subset(data.1b, Year_End   <= 1939)
data.1c  <- data.1a[-c(109:133), ]
data.2a  <- read.csv(file = "Hobo_Trips2.csv", head = TRUE, sep = ",")
data.2b  <- subset(data.2a, Year_Start >= 1929)
data.2b  <- subset(data.2b, Year_End   <= 1939)
data.2c  <- read.csv(file = "Hobo_Trips2c.csv", head = TRUE, sep = ",")
data.3a  <- read.csv(file = "Hobo_Trips3a.csv", head = TRUE, sep = ",")
data.3b  <- read.csv(file = "Hobo_Trips3b.csv", head = TRUE, sep = ",")

###############################################################
### Create base map

# Login to Google account
ggmap::register_google(key = "AIzaSyA92NkRZlE1M0ZV5Y1dcEIje0zcCFAHoTI")

# Create Google map
google.map <- get_googlemap(center = c(lon = -100, lat = 45), zoom = 3, scale = 2, maptype ='roadmap', color = "color")

# Create state outlines
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
map <- ne_countries(scale = "medium", returnclass = "sf")

# Reformat data
data.1a$Lat        <- as.numeric(as.character(data.1a$Lat))
data.1a$Long       <- as.numeric(as.character(data.1a$Long))
data.1a$Individual <- as.factor(data.1a$Individual)
data.1a$City       <- as.factor(data.1a$City)
data.1a$Year       <- as.factor(data.1a$Year)

# Subset data for each individual
hobo.1        <- subset(data.1a, Individual == "1")
hobo.1.points <- nrow(hobo.1)
hobo.2        <- subset(data.1a, Individual == "2")
hobo.2.points <- nrow(hobo.2)
hobo.7        <- subset(data.1a, Individual == "7")
hobo.7.points <- nrow(hobo.7)
hobo.8        <- subset(data.1a, Individual == "8")
hobo.8.points <- nrow(hobo.8)
hobo.9        <- subset(data.1a, Individual == "9")
hobo.9.points <- nrow(hobo.9)
hobo.14        <- subset(data.1a, Individual == "14")
hobo.14.points <- nrow(hobo.14)
hobo.15        <- subset(data.1a, Individual == "15")
hobo.15.points <- nrow(hobo.15)
hobo.16        <- subset(data.1a, Individual == "16")
hobo.16.points <- nrow(hobo.16)
hobo.19        <- subset(data.1a, Individual == "19")
hobo.19.points <- nrow(hobo.19)
hobo.23        <- subset(data.1a, Individual == "23")
hobo.23.points <- nrow(hobo.23)
hobo.24        <- subset(data.1a, Individual == "24")
hobo.24.points <- nrow(hobo.24)
hobo.25        <- subset(data.1a, Individual == "25")
hobo.25.points <- nrow(hobo.25)
hobo.26        <- subset(data.1a, Individual == "26")
hobo.26.points <- nrow(hobo.26)
hobo.27        <- subset(data.1a, Individual == "27")
hobo.27.points <- nrow(hobo.27)
hobo.28        <- subset(data.1a, Individual == "28")
hobo.28.points <- nrow(hobo.28)
hobo.29        <- subset(data.1a, Individual == "29")
hobo.29.points <- nrow(hobo.29)
hobo.33        <- subset(data.1a, Individual == "33")
hobo.33.points <- nrow(hobo.33)
hobo.35        <- subset(data.1a, Individual == "35")
hobo.35.points <- nrow(hobo.35)
hobo.36        <- subset(data.1a, Individual == "36")
hobo.36.points <- nrow(hobo.36)
hobo.37        <- subset(data.1a, Individual == "37")
hobo.37.points <- nrow(hobo.37)
hobo.38        <- subset(data.1a, Individual == "38")
hobo.38.points <- nrow(hobo.38)
hobo.39        <- subset(data.1a, Individual == "39")
hobo.39.points <- nrow(hobo.39)
hobo.40        <- subset(data.1a, Individual == "40")
hobo.40.points <- nrow(hobo.40)
hobo.42        <- subset(data.1a, Individual == "42")
hobo.42.points <- nrow(hobo.42)
hobo.43        <- subset(data.1a, Individual == "43")
hobo.43.points <- nrow(hobo.43)
hobo.44        <- subset(data.1a, Individual == "44")
hobo.44.points <- nrow(hobo.44)
hobo.45        <- subset(data.1a, Individual == "45")
hobo.45.points <- nrow(hobo.45)
hobo.46        <- subset(data.1a, Individual == "46")
hobo.46.points <- nrow(hobo.46)
hobo.50        <- subset(data.1a, Individual == "50")
hobo.50.points <- nrow(hobo.50)

###############################################################
### Figure 1a: Google map w/ all localities (1885 - 1946)

#pdf("hobo_fig1a.pdf", width = 12, height = 10)
ggmap(google.map) +
  geom_point(data = data.1a, aes(x = Long, y = Lat, fill = factor(Name)), color = "black", shape = 21, size = 2) +
  labs(x = "Longitude", y = "Latitude", title = "Hobos of the United States (1885 - 1946)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme(plot.title = element_text(face = "bold", size = 51, margin = margin(0, 0, 20 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)),
        legend.background = element_rect(color = "black", fill = "ivory", size = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(10, 20, 10, 10),
        legend.box.margin = margin(0, 0, 0, 20))
dev.off()

###############################################################
### Figure 1b: State map w/ all localities (1885 - 1946)

pdf("hobo_fig1b.pdf", width = 12, height = 10)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.5) + 
  geom_point(data = data.1c, aes(x = Long, y = Lat, fill = factor(Name)), color = "black", shape = 21, size = 3) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = "Hobos of the United States (1885 - 1946)") +
  guides(fill = guide_legend(ncol = 2)) +
  theme(plot.title = element_text(face = "bold", size = 48, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        text = element_text(size = 12),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)),
        legend.background = element_rect(color = "black", fill = "ivory", size = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(10, 20, 10, 10),
        legend.box.margin = margin(0, 0, 0, 20),
        legend.text = element_text(size = 8))
dev.off()

###############################################################
### Figure 1c: Google map w/ all localities (1929 - 1939)

pdf("hobo_fig1c.pdf", width = 12, height = 9)
ggmap(google.map) +
  geom_point(data = data.1b, aes(x = Long, y = Lat, fill = factor(Name)), color = "black", shape = 21, size = 2) +
  labs(x = "Longitude", y = "Latitude", title = "Hobos of the United States (1929 - 1939)") +
  guides(fill = guide_legend(ncol = 1)) +
  theme(plot.title = element_text(face = "bold", size = 46, margin = margin(0, 0, 20 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)),
        legend.background = element_rect(color = "black", fill = "ivory", size = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(10, 20, 10, 10),
        legend.box.margin = margin(0, 0, 0, 20))
dev.off()

###############################################################
### Figure 1d: State map w/ all localities (1929 - 1939)

pdf("hobo_fig1d.pdf", width = 12, height = 7)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.5) + 
  geom_point(data = data.1b, aes(x = Long, y = Lat, fill = factor(Name)), color = "black", shape = 21, size = 3) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = "Hobos of the United States (1929 - 1939)") +
  guides(fill = guide_legend(ncol = 2)) +
  theme(plot.title = element_text(face = "bold", size = 48, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        text = element_text(size = 12),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)),
        legend.background = element_rect(color = "black", fill = "ivory", size = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(10, 20, 10, 10),
        legend.box.margin = margin(0, 0, 0, 20),
        legend.text = element_text(size = 8))

dev.off()

###############################################################
### Figure 2: GIF map for each individual (last image is one option for pathway map for each individual)

# Base map for transitions (to add in between GIFs if you want to combine maps for several individuals)
pdf("hobo_fig2_0.pdf", width = 12, height = 12)
ggmap(google.map) + labs(x = "Longitude", y = "Latitude")
dev.off()

# Individual 1
plotfoo <- function(){
  for(i in 1:hobo.1.points){
    take_df <- hobo.1[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.1$Name, "(", hobo.1$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.1.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_1.gif", interval = 2)

# Individual 2
plotfoo <- function(){
  for(i in 1:hobo.2.points){
    take_df <- hobo.2[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.2$Name,"(", hobo.2$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.2.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_2.gif", interval = 2)

# Individual 7
plotfoo <- function(){
  for(i in 1:hobo.7.points){
    take_df <- hobo.7[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.7$Name,"(", hobo.7$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.7.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_7.gif", interval = 2)

# Individual 8
plotfoo <- function(){
  for(i in 1:hobo.8.points){
    take_df <- hobo.8[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.8$Name,"(", hobo.8$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.8.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_8.gif", interval = 2)

# Individual 9
plotfoo <- function(){
  for(i in 1:hobo.9.points){
    take_df <- hobo.9[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.9$Name,"(", hobo.9$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.9.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_9.gif", interval = 2)

# Individual 14
plotfoo <- function(){
  for(i in 1:hobo.14.points){
    take_df <- hobo.14[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.14$Name,"(", hobo.14$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.14.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_14.gif", interval = 2)

# Individual 15
plotfoo <- function(){
  for(i in 1:hobo.15.points){
    take_df <- hobo.15[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.15$Name,"(", hobo.15$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.15.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_15.gif", interval = 2)

# Individual 16
plotfoo <- function(){
  for(i in 1:hobo.16.points){
    take_df <- hobo.16[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.16$Name,"(", hobo.16$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.16.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_16.gif", interval = 2)

# Individual 19
plotfoo <- function(){
  for(i in 1:hobo.19.points){
    take_df <- hobo.19[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.19$Name,"(", hobo.19$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.19.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_19.gif", interval = 2)

# Individual 23
plotfoo <- function(){
  for(i in 1:hobo.23.points){
    take_df <- hobo.23[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.23$Name,"(", hobo.23$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.23.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_23.gif", interval = 2)

# Individual 24
plotfoo <- function(){
  for(i in 1:hobo.24.points){
    take_df <- hobo.24[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.24$Name,"(", hobo.24$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.24.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_24.gif", interval = 2)

# Individual 25
plotfoo <- function(){
  for(i in 1:hobo.25.points){
    take_df <- hobo.25[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.25) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.25$Name,"(", hobo.25$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.25.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_25.gif", interval = 2)

# Individual 26
plotfoo <- function(){
  for(i in 1:hobo.26.points){
    take_df <- hobo.26[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.26) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.26$Name,"(", hobo.26$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.26.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_26.gif", interval = 2)

# Individual 27
plotfoo <- function(){
  for(i in 1:hobo.27.points){
    take_df <- hobo.27[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.27) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.27$Name,"(", hobo.27$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.27.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_27.gif", interval = 2)

# Individual 28
plotfoo <- function(){
  for(i in 1:hobo.28.points){
    take_df <- hobo.28[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.28) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.28$Name,"(", hobo.28$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.28.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_28.gif", interval = 2)

# Individual 29
plotfoo <- function(){
  for(i in 1:hobo.29.points){
    take_df <- hobo.29[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.29) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.29$Name,"(", hobo.29$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.29.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_29.gif", interval = 2)

# Individual 33
plotfoo <- function(){
  for(i in 1:hobo.33.points){
    take_df <- hobo.33[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.33) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.33$Name,"(", hobo.33$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.33.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_33.gif", interval = 2)

# Individual 35
plotfoo <- function(){
  for(i in 1:hobo.35.points){
    take_df <- hobo.35[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.35) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.35$Name,"(", hobo.35$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.35.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_35.gif", interval = 2)

# Individual 36
plotfoo <- function(){
  for(i in 1:hobo.36.points){
    take_df <- hobo.36[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.36) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.36$Name,"(", hobo.36$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.36.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_36.gif", interval = 2)

# Individual 37
plotfoo <- function(){
  for(i in 1:hobo.37.points){
    take_df <- hobo.37[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.37) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.37$Name,"(", hobo.37$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.37.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_37.gif", interval = 2)

# Individual 38
plotfoo <- function(){
  for(i in 1:hobo.38.points){
    take_df <- hobo.38[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.38) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.38$Name,"(", hobo.38$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.38.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_38.gif", interval = 2)

# Individual 39
plotfoo <- function(){
  for(i in 1:hobo.39.points){
    take_df <- hobo.39[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.39) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.39$Name,"(", hobo.39$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.39.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_39.gif", interval = 2)

# Individual 40
plotfoo <- function(){
  for(i in 1:hobo.40.points){
    take_df <- hobo.40[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.40) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.40$Name,"(", hobo.40$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.40.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_40.gif", interval = 2)

# Individual 42
plotfoo <- function(){
  for(i in 1:hobo.42.points){
    take_df <- hobo.42[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.42) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.42$Name,"(", hobo.42$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.42.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_42.gif", interval = 2)

# Individual 43
plotfoo <- function(){
  for(i in 1:hobo.43.points){
    take_df <- hobo.43[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.43) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.43$Name,"(", hobo.43$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.43.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_43.gif", interval = 2)

# Individual 44
plotfoo <- function(){
  for(i in 1:hobo.44.points){
    take_df <- hobo.44[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.44) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.44$Name,"(", hobo.44$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.44.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_44.gif", interval = 2)

# Individual 45
plotfoo <- function(){
  for(i in 1:hobo.45.points){
    take_df <- hobo.45[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.45) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.45$Name,"(", hobo.45$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.45.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_45.gif", interval = 2)

# Individual 46
plotfoo <- function(){
  for(i in 1:hobo.46.points){
    take_df <- hobo.46[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.46) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.46$Name,"(", hobo.46$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.46.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_46.gif", interval = 2)

# Individual 50
plotfoo <- function(){
  for(i in 1:hobo.50.points){
    take_df <- hobo.50[1:i, ]
    p <- ggmap(google.map) +
      geom_path(data = take_df, aes(x = Long, y = Lat), lineend = "round", size = 1) +
      geom_point(data = take_df, aes(x = Long, y = Lat), shape = 21, fill = "darkgoldenrod2", color = "black", size = 4) +
      geom_label_repel(data = take_df, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5,
                 inherit.aes = FALSE, label.size = 0.50) +
      labs(x = "Longitude", y = "Latitude", title = paste(hobo.50$Name,"(", hobo.50$Year, ")")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 42, margin = margin(0, 0, 30 ,0), family = "Trattatello"),
            plot.background = element_rect(fill = 'ivory2', color = 'black'),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            panel.background = element_rect(fill = "ivory"),
            panel.border = element_rect(color = "black", fill = "NA", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 14),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.title = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
            axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
    print(p)
  }
}

oopt = ani.options(interval = 1, nmax = hobo.50.points, ppi = 600, ani.height = 750, ani.width = 750)
saveGIF(plotfoo(), movie.name = "hobo_fig2_50.gif", interval = 2)

###############################################################
### Figure 3a: Interactive map for all individuals (1885 - 1946)

# Create North America map for interactive figure
geo.na <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("ivory2"),
  countrycolor = toRGB("black")
)

# Create interactive figure for all individuals
all.interactive.a <- plot_geo(locationmode = 'USA-states') %>%
  add_segments(
    data = group_by(data.2a, Individual),
    x = ~Start_Long, xend = ~End_Long,
    y = ~Start_Lat, yend = ~End_Lat,
    alpha = 0.3, size = I(1), hoverinfo = "none", color = I("black")
  ) %>%
  add_markers(
    data = data.1a, x = ~Long, y = ~Lat,
    text = paste(data.1a$Name, "<br>", data.1a$City, "<br>","( Stop:", data.1a$Stop, ")"),
    size = 1, hoverinfo = "text", alpha = 0.8, color = I(~Individual)
  ) %>%
  layout(
    title = list(
      text = "<b> Hobos of the United States (1885 - 1946) </b>"),
      font = list(
        family = "Balto",
        size = 24),
    margin = list(t = 50),
    showlegend = FALSE,
    geo = geo.na
  )

# Show interactive figure
all.interactive.a

# Save interactive figure
htmlwidgets::saveWidget(as_widget(all.interactive.a), "hobo_fig3a_all.html")

###############################################################
### Figure 3b: Interactive map for all individuals (1929 - 1939)

# Create US map for interactive figure
geo.us <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'),
  showland = TRUE,
  landcolor = toRGB("gray90"),
  countrycolor = toRGB("white")
)

# Create interactive figure for all individuals
all.interactive.b <- plot_geo(locationmode = 'USA-states') %>%
  add_segments(
    data = group_by(data.2b, Individual),
    x = ~Start_Long, xend = ~End_Long,
    y = ~Start_Lat, yend = ~End_Lat,
    alpha = 0.3, size = I(1), hoverinfo = "none", color = I("black")
  ) %>%
  add_markers(
    data = data.1b, x = ~Long, y = ~Lat,
    text = paste(data.1b$Name, "<br>", data.1b$City, "<br>","( Stop:", data.1b$Stop, ")"),
    size = 1.5, hoverinfo = "text", alpha = 0.9, color = I(~Individual)
  ) %>%
  layout(
    title = list(
      text = "<b> Hobos of the United States (1885 - 1946) </b>"),
    font = list(
      family = "Balto",
      size = 24),
    margin = list(t = 50),
    showlegend = FALSE,
    geo = geo.us
  )

# Show interactive figure
all.interactive.b

# Save interactive figure
htmlwidgets::saveWidget(as_widget(all.interactive.b), "hobo_fig3b_1930s.html")

###############################################################
### Figure 3c: Interactive map for all individuals (1929 - 1939)

# Create interactive figure for all individuals
all.interactive.1930s <- plot_geo(locationmode = 'USA-states') %>%
  add_markers(
    data = data.1b, x = ~Long, y = ~Lat,
    text = paste(data.1b$Name, "<br>", data.1b$City, "<br>","( Stop:", data.1b$Stop, ")"),
    size = 2, hoverinfo = "text", alpha = 0.8, color = I(~Individual),
  ) %>%
  layout(
    title = list(
      text = "<b> Hobos of the United States (1929 - 1939) </b>"),
    font = list(
      family = "Balto",
      size = 24),
    legend = list(
      font = list(
        size = 20)),
    margin = list(t = 50),
    showlegend = FALSE,
    geo = geo.us
  )

# Show interactive figure
all.interactive.1930s

# Save interactive figure
htmlwidgets::saveWidget(as_widget(all.interactive.1930s), "hobo_fig3c_1930s.html")

###############################################################
### Figure 3.8: Example interactive map for one individual

# Subset data
hobo.8a <- subset(data.1a, Individual == "8")
hobo.8b <- subset(data.2a, Individual == "8")

# Create interactive figure
hobo.8.interactive <- plot_geo(locationmode = 'USA-states') %>%
  add_segments(
    data = group_by(hobo.8b, Individual),
    x = ~Start_Long, xend = ~End_Long,
    y = ~Start_Lat, yend = ~End_Lat,
    alpha = 0.8, size = I(2), hoverinfo = "none", color = I("cadetblue4")
  ) %>%
  add_markers(
    data = hobo.8a, x = ~Long, y = ~Lat,
    text = paste(hobo.8a$Name, "<br>", hobo.8a$City, "<br>","( Stop:", hobo.8a$Stop, ")"),
    size = 4, hoverinfo = "text", alpha = 0.8, color = I("cadetblue2")
  ) %>%
  layout(
    title = list(
      text = "<b> Harold A. Alfter (1932 - 1937) </b>"),
    font = list(
      family = "Balto",
      size = 24),
    legend = list(
      font = list(
        size = 20)),
    margin = list(t = 50),
    showlegend = FALSE,
    geo = geo.us
  )

# Show interactive figure
hobo.8.interactive

# Save interactive figure
htmlwidgets::saveWidget(as_widget(hobo.8.interactive), "hobo_fig3_8.html")

###############################################################
### Figure 4: State map with pathways for each individual (1885 - 1946)

# Individual 1
pdf("hobo_fig4_1.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.1, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.1, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.1, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.1$Name,"(", hobo.1$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 2
pdf("hobo_fig4_2.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.2, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.2, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.2, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.2$Name,"(", hobo.2$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 7
pdf("hobo_fig4_7.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.7, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.7, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.7, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.7$Name,"(", hobo.7$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 8
pdf("hobo_fig4_8.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.8, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.8, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.8, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.8$Name,"(", hobo.8$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 9
pdf("hobo_fig4_9.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.9, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.9, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.9, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.9$Name,"(", hobo.9$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 14
pdf("hobo_fig4_14.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.14, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.14, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.14, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.14$Name,"(", hobo.14$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 15
pdf("hobo_fig4_15.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.15, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.15, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.15, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.15$Name,"(", hobo.15$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 16
pdf("hobo_fig4_16.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.16, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.16, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.16, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.16$Name,"(", hobo.16$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 19
pdf("hobo_fig4_19.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.19, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.19, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.19, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.19$Name,"(", hobo.19$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 23
pdf("hobo_fig4_23.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.23, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.23, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.23, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.23$Name,"(", hobo.23$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 24
pdf("hobo_fig4_24.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.24, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.24, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.24, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.24$Name,"(", hobo.24$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 25
pdf("hobo_fig4_25.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.25, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.25, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.25, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.25$Name,"(", hobo.25$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 26
pdf("hobo_fig4_26.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.26, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.26, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.26, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.26$Name,"(", hobo.26$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 27
pdf("hobo_fig4_27.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.27, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.27, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.27, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.27$Name,"(", hobo.27$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 28
pdf("hobo_fig4_28.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.28, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.28, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.28, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.28$Name,"(", hobo.28$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 29
pdf("hobo_fig4_29.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.29, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.29, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.29, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.29$Name,"(", hobo.29$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 33
pdf("hobo_fig4_33.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.33, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.33, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.33, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.33$Name,"(", hobo.33$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 35
pdf("hobo_fig4_35.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.35, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.35, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.35, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.35$Name,"(", hobo.35$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 36
pdf("hobo_fig4_36.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.36, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.36, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.36, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.36$Name,"(", hobo.36$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 37
pdf("hobo_fig4_37.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.37, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.37, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.37, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.37$Name,"(", hobo.37$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 38
pdf("hobo_fig4_38.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.38, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.38, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.38, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.38$Name,"(", hobo.38$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 39
pdf("hobo_fig4_39.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.39, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.39, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.39, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.39$Name,"(", hobo.39$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 42
pdf("hobo_fig4_42.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.42, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.42, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.42, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.42$Name,"(", hobo.42$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 44
pdf("hobo_fig4_44.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.44, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.44, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.44, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.44$Name,"(", hobo.44$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 45
pdf("hobo_fig4_45.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.45, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.45, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.45, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.45$Name,"(", hobo.45$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 46
pdf("hobo_fig4_46.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.46, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.46, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.46, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.46$Name,"(", hobo.46$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

# Individual 50
pdf("hobo_fig4_50.pdf", width = 12, height = 8)
ggplot(data = map) +
  geom_sf(data = states, fill = "lemonchiffon4", color = "ivory", size = 0.25) + 
  geom_path(data = hobo.50, aes(x = Long, y = Lat), color = "cadetblue2", size = 1) +
  geom_point(data = hobo.50, aes(x = Long, y = Lat), fill = "cadetblue2", color = "black", shape = 21, size = 4) +
  geom_label_repel(data = hobo.50, aes(x = Long, y = Lat, label = City), hjust = 0.0, vjust = -0.5, inherit.aes = FALSE, label.size = 0.43) +
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = paste(hobo.50$Name,"(", hobo.50$Year, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

###############################################################
### Figure 5a: Density map by state (1885 - 1946)

# Merge state maps with counts of individuals
data.states.a <- merge(states, data.3a, by = "ID", all.x = TRUE, all.y = TRUE)

# Create figure
pdf("hobo_fig5a.pdf", width = 12, height = 7)
ggplot(data = map) +
  geom_sf(data = data.states.a, aes(fill = factor(Count))) + 
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  scale_fill_brewer(breaks = c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), labels = c("11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"), palette = "YlOrRd") +
  labs(x = "Longitude", y = "Latitude", fill = "Individuals", title = "Hobos of the United States (1885 - 1946)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory", color = "black"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        legend.background = element_rect(fill = "ivory", color = "black", size = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

###############################################################
### Figure 5b: Density map by state (1929 - 1939)

# Merge state maps with counts of individuals
data.states.b <- merge(states, data.3b, by = "ID", all.x = TRUE, all.y = TRUE)

# Create figure
pdf("hobo_fig5b.pdf", width = 12, height = 7)
ggplot(data = map) +
  geom_sf(data = data.states.b, aes(fill = factor(Count))) + 
  coord_sf(xlim = c(-130, -63), ylim = c(23, 53), expand = FALSE) +
  scale_fill_brewer(breaks = c(9, 8, 7, 6, 5, 4, 3, 2, 1), labels = c("9", "8", "7", "6", "5", "4", "3", "2", "1"), palette = "YlOrRd") +
  labs(x = "Longitude", y = "Latitude", fill = "Individuals", title = "Hobos of the United States (1929 - 1939)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 36, margin = margin(0, 0, 5 ,0), family = "Trattatello"),
        plot.background = element_rect(fill = 'ivory2', color = 'black'),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(fill = "ivory", color = "black"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        legend.background = element_rect(fill = "ivory", color = "black", size = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0)))
dev.off()

###############################################################
###############################################################
###############################################################



