# by Christophe Loth
# Written on August 3, 2014
# This code fetches JSON data from the USGS website and maps past earthquakes in a user-defined time window.

# Inputs:
#   - startdate: date/string type in YYYY-MM-DD format 
#   - enddate: date/string type in YYYY-MM-DD format
#   - minmagnitude: minimum considered magnitude (float)

# Output:
#   - a pdf world map showing the earthquake occurrences in the input time window





# Loading packages
library(rjson)
library(ggplot2)
library(grid)


##########################################################################################################
# User inputs
# Time window: input dates are in YYYY-MM-DD format (enddate>startdate)
startdate = "2009-12-01"   
enddate = "2014-12-01"
minmagnitude = 5 #magnitude threshold
##########################################################################################################


# Fetch json data from USGS website
json_file = paste("http://comcat.cr.usgs.gov/fdsnws/event/1/query?starttime=",startdate,
          "%2000:00:00&minmagnitude=",toString(minmagnitude),
          "&format=geojson&endtime=",enddate,
          "%2023:59:59&orderby=time",sep="")


data = fromJSON(file=json_file)



# Import data into catalog data frame
nEQ=as.numeric(summary(data["features"])[1])

catalog=as.data.frame(matrix(ncol=3,nrow=nEQ))
names(catalog)=c("Magnitude","Longitude","Latitude")

for (i in 1:nEQ){
  catalog$Magnitude[i] = data$features[[i]]$properties$mag
  catalog$Longitude[i] = data$features[[i]]$geometry$coordinates[1]
  catalog$Latitude[i] = data$features[[i]]$geometry$coordinates[2]
}


# Reorder the catalog by earthquake magnitude
catalog = catalog[order(catalog$Magnitude),]


# Plot map of recorded earthquakes 
pdf(file = paste("mapEQsFrom",startdate,"To",enddate,".pdf", sep = ""),bg = 'navyblue',width = 15, height=7)

print(ggplot()+
        geom_polygon(aes(x = long, y = lat, group = group), 
                     data = map_data("world"),fill = 'gray51',alpha=0.75)+
        geom_point(aes(x=Longitude,y=Latitude,size=Magnitude, color=Magnitude),data=catalog)+ 
        scale_size_continuous(name="Magnitude",range = c(1,5))+ #point size
        scale_color_continuous(name="Magnitude",low="green",high="red")+ #point color
        theme_classic()+
        theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
              plot.margin=unit(c(-1, -1, -6.5, -3.5),"mm"),panel.background = element_rect(fill='navyblue'),
              axis.title.y=element_blank(),axis.title.x=element_blank(),
              legend.justification = c(0, 1),legend.position=c(0,1),
              legend.title = element_text(size=20,color="white"),
              legend.text = element_text(size = 18,color="white"),
              legend.text.align=0,legend.background = element_rect(fill="transparent"))+
        geom_text(aes(x=32,y=-72),label=paste("Recorded earthquakes from ",startdate," to ",enddate,sep=""),
                  color="white",hjust=0,size=6)+
        guides(color = guide_legend())+
        coord_fixed(ylim = c(-80, 85), xlim = c(-175, 185))
)

dev.off()

