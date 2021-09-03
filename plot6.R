if(!file.exists("summarySCC_PM25.rds")| !file.exists("Source_Classification_Code.rds")) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
    unzip("data.zip")
}
data<-readRDS("summarySCC_PM25.rds")
code<-readRDS("Source_Classification_Code.rds")

if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
library(dplyr)
motor<-as.character(code[c(grepl("motor", code$SCC.Level.Three, ignore.case = T),
                           grepl("motorcycles", code$SCC.Level.Four, ignore.case = T)),1])

mot<-filter(data, fips == "24510" | fips=="06037", SCC %in% motor)
mot$fips<-as.factor(mot$fips)
levels(mot$fips)<-c("LA County", "Baltimore City")
summ<-group_by(mot, fips, year) %>% summarize(total=sum(Emissions))

if(!("ggplot2" %in% installed.packages())) install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
f<-qplot(year, total, data=summ, ylab="Total PM2.5 Emissions (tons)",
         colour=fips) +geom_line(lty=2) + geom_smooth(method="lm", se=F)+ ggtitle(
             "PM2.5 Emission from motor vehicles \nLA and Baltimore comparison")

g<-qplot(year,log(Emissions), data=mot,
facets=.~fips) +geom_boxplot(
aes(group=year))+ geom_point(
aes(colour=type)) +scale_x_continuous(breaks=mot$year) + ggtitle(" \n ") + scale_color_brewer(palette="Set1")

ggsave("plot6.png", width=10, arrangeGrob(f,g,ncol=2))
