if(!file.exists("summarySCC_PM25.rds")) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
    unzip("data.zip")
}
data<-readRDS("summarySCC_PM25.rds")

if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
library(dplyr)
bal<-filter(data, fips == "24510")
summ<- bal %>% group_by(type, year) %>% summarise(total_per_type=sum(Emissions))

if(!("ggplot2" %in% installed.packages())) install.packages("ggplot2")
library(ggplot2)
library(gridExtra)

f<-ggplot(bal, aes(year, log10(Emissions+1)))+geom_boxplot(aes(group=year))+facet_wrap(~type, nrow=1)+ xlab(element_blank())+ scale_x_continuous(breaks=NULL) +ggtitle("PM2.5 Emissions in Baltimore City by type of source", subtitle = "By individual sources")

g<-ggplot(summ, aes(year, total_per_type)) + geom_smooth(method=lm, se=F)+ geom_point()+ facet_wrap(~type, nrow=1)+ theme(strip.background = element_blank(), strip.text = element_blank())+ ggtitle(NULL,subtitle="Total per year")+ ylab("PM2.5 emissions (tons)")

ggsave("plot3.png", arrangeGrob(f,g,nrow=2))