suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggthemes))
library(showtext)
library(reshape2)
library(tidyverse)
library(classInt)

setwd("~/Desktop/GLP/R Code/Emp Data")

####################
#CBP Data 2003-2015#
####################

cbp_03 = read.csv("cbp03msa.txt", header = TRUE)
cbp_03$Year = 2003

cbp_04 = read.csv("cbp04msa.txt", header = TRUE)
cbp_04$Year = 2004

cbp_05 = read.csv("cbp05msa.txt", header = TRUE)
cbp_05$Year = 2005

cbp_06 = read.csv("cbp06msa.txt", header = TRUE)
cbp_06$Year = 2006

cbp_07 = read.csv("cbp07msa.txt", header = TRUE)
cbp_07$Year = 2007

cbp_08 = read.csv("cbp08msa.txt", header = TRUE)
cbp_08$Year = 2008

cbp_09 = read.csv("cbp09msa.txt", header = TRUE)
cbp_09$Year = 2009

cbp_10 = read.csv("cbp10msa.txt", header = TRUE)
cbp_10$Year = 2010

cbp_11 = read.csv("cbp11msa.txt", header = TRUE)
cbp_11$Year = 2011

cbp_12 = read.csv("cbp12msa.txt", header = TRUE)
cbp_12$Year = 2012

cbp_13 = read.csv("cbp13msa.txt", header = TRUE)
cbp_13$Year = 2013

cbp_14 = read.csv("cbp14msa.txt", header = TRUE)
cbp_14$Year = 2014

cbp_15 = read.csv("cbp15msa.txt", header = TRUE)
cbp_15$Year = 2015

pull_peers_MSA<-function(data){
  all.peers<-subset(data, data$MSA == 24340 | data$MSA == 41180
                    |data$MSA == 36420 | data$MSA == 46140
                    |data$MSA == 24860 |data$MSA == 28940
                    |data$MSA == 13820 |data$MSA == 26900
                    |data$MSA == 31140 |data$MSA == 28140
                    |data$MSA == 36540 |data$MSA == 24660
                    |data$MSA == 16740 |data$MSA == 18140
                    |data$MSA == 17140 |data$MSA == 34980
                    |data$MSA == 32820 |data$MSA == 27260
                    |data$MSA == 39580 |data$MSA == 19380
                    |data$MSA == 40060)
  all.peers$baseline<-1
  all.peers$current<-1
  all.peers$baseline[all.peers$MSA == 24340 | all.peers$MSA == 41180
                     |all.peers$MSA == 36420 | all.peers$MSA == 46140
                     |all.peers$MSA == 24860 | all.peers$MSA == 28940]<-0
  all.peers$current[all.peers$MSA == 27260 | all.peers$MSA == 39580
                    |all.peers$MSA == 19380 | all.peers$MSA == 40060]<-0
  all.peers
}

econ_data_format<-function(data){
  if(colnames(data)[1] == "msa"){
    data = rename(data, MSA = msa, NAICS = naics, 
                  EMPFLAG = empflag, EMP = emp,
                  QP1 = qp1, AP = ap, EST = est,
                  N1_4 = n1_4, N5_9 = n5_9, N10_19 = n10_19,
                  N20_49 = n20_49, N50_99 = n50_99, N100_249 = n100_249,
                  N250_499 = n250_499, N500_999 = n500_999,
                  N1000 = n1000, N1000_1 = n1000_1, N1000_2 = n1000_2,
                  N1000_3 = n1000_3, N1000_4 = n1000_4)
  }
  data = select(data, MSA, NAICS, EMP, EMPFLAG, QP1, AP, EST, N1_4,N5_9,
                N10_19, N20_49, N50_99, N100_249, N250_499, N500_999,
                N1000, N1000_1, N1000_2, N1000_3, N1000_4, Year)
  data = pull_peers_MSA(data)
  data = subset(data,data$NAICS == "11----"| data$NAICS == "21----"
                    |data$NAICS == "22----" | data$NAICS == "23----"
                    |data$NAICS == "31----" | data$NAICS == "32----"
                    |data$NAICS == "33----" | data$NAICS == "42----"
                    |data$NAICS == "44----" | data$NAICS == "45----"
                    |data$NAICS == "48----" | data$NAICS == "49----"
                    |data$NAICS == "51----" | data$NAICS == "52----"
                    |data$NAICS == "53----" |data$NAICS == "54----"
                    |data$NAICS == "55----" |data$NAICS == "56----"
                    |data$NAICS == "61----" |data$NAICS == "62----"
                    |data$NAICS == "71----" |data$NAICS == "72----"
                    |data$NAICS == "81----" |data$NAICS == "92----"
                    |data$NAICS == "------")
  data
}

cbp_03 = econ_data_format(cbp_03)
cbp_04 = econ_data_format(cbp_04)
cbp_05 = econ_data_format(cbp_05)
cbp_06 = econ_data_format(cbp_06)
cbp_07 = econ_data_format(cbp_07)
cbp_08 = econ_data_format(cbp_08)
cbp_09 = econ_data_format(cbp_09)
cbp_10 = econ_data_format(cbp_10)
cbp_11 = econ_data_format(cbp_11)
cbp_12 = econ_data_format(cbp_12)
cbp_13 = econ_data_format(cbp_13)
cbp_14 = econ_data_format(cbp_14)
cbp_15 = econ_data_format(cbp_15)

final_cbp<-rbind(cbp_03,cbp_04)
final_cbp<-final_cbp%>%
  rbind(cbp_05)%>%
  rbind(cbp_06)%>%
  rbind(cbp_07)%>%
  rbind(cbp_08)%>%
  rbind(cbp_09)%>%
  rbind(cbp_10)%>%
  rbind(cbp_11)%>%
  rbind(cbp_12)%>%
  rbind(cbp_13)%>%
  rbind(cbp_14)%>%
  rbind(cbp_15)

View(final_cbp)

write.csv(final_cbp, file = "GLP_CBP_data.csv", row.names = FALSE)

#Top Five Industries by City
row_check_EMP<-function(x, data, msa, start_year){
  working = TRUE
  y = x
  while(working){
    if((y$EMP == 0 & y$EMPFLAG != "")){
      start_year = start_year + 1
      y = filter(data, MSA == msa, Year == start_year, NAICS == x[1,2])
    }
    else{
      x$EMP = y$EMP
      x$EMPFLAG = y$EMPFLAG
      x$start_growth_year = start_year
      working = FALSE
    }
  }
  x
}


#Returns dataset with the growth rate by employment for the Top Five Industries 
#as well as the overall growth rate for a given time interval.
top_five_industries_emp<-function(data, msa, start_year, end_year){
  df = subset(data, MSA == msa & Year == start_year)
  df = select(df, MSA, NAICS,EMP, EMPFLAG)
  q = data.frame(MSA = numeric(), NAICS=factor(), EMP = numeric(),
                 EMPFLAG = factor(), start_growth_year = numeric(), 
                 stringsAsFactors = TRUE)
  for(i in c(1:nrow(df))){
    r = row_check_EMP(df[i,], final_cbp, 31140, 2003)
    q = rbind(q,r)
  }
  q$start_growth = q$EMP
  q$end_growth = subset(data, MSA == msa & Year == end_year)$EMP
  q = mutate(q,pct_growth = ((end_growth - start_growth)/start_growth)*100)
  q = select(q,MSA, NAICS, start_growth_year, start_growth, end_growth, pct_growth)
  q = arrange(q, desc(pct_growth))
  q = rbind(q[1:5,], filter(q, NAICS == '------'))
  q
}

top_five_industries_est<-function(data, msa, start_year, end_year){
  df = subset(data, MSA == msa & Year == start_year)
  df = select(df, MSA, NAICS,EST, Year)
  df = rename(df, start_growth = EST, start_growth_year = Year)
  df$end_growth = subset(data, MSA == msa & Year == end_year)$EST
  df = mutate(df,pct_growth = ((end_growth - start_growth)/start_growth)*100)
  df = select(df,MSA, NAICS, start_growth_year, start_growth, end_growth, pct_growth)
  df = arrange(df, desc(pct_growth))
  df = rbind(df[1:5,], filter(df, NAICS == '------'))
  df
}


louis_emp=top_five_industries_emp(final_cbp, 31140, 2003, 2015)
louis_est = top_five_industries_est(final_cbp, 31140, 2003, 2015)

#Test to see if industries are the same or not
all(sort(louis_est$NAICS)==sort(louis_est$NAICS))


#Slopegraph of Individual Industries
create_slope_graph<-function(df){
  l11<-paste(df$NAICS, df$start_growth, sep="\n")
  l13<-paste(df$NAICS, df$end_growth, sep="\n")
  p<-ggplot(df) + geom_segment(aes(x = 2003, xend = 2015, y = df$start_growth, yend = df$end_growth, colour = NAICS))
  p<-p + guides(colour = FALSE)
  p<-p + theme_tufte()
  p<-p + ggtitle("Five Fastest Growing Economic Sectors in Louisville")
  p<-p + theme(plot.title = element_text(hjust = 0.5, vjust = -2.5))
  p<-p + theme(panel.background = element_blank())
  p<-p + theme(panel.grid=element_blank())
  p<-p + theme(axis.ticks=element_blank())
  p<-p + theme(axis.text=element_blank())
  p<-p + theme(panel.border=element_blank())
  p<-p + xlab("") + ylab("Firms")
  # p<-p + theme(axis.title.y = element_text(vjust = -20))
  p<-p + xlim((2003-1),(2015+1))
  p<-p + ylim(0,(1.1*(max(df$start_growth,df$end_growth))))
  p<-p + geom_text(label=l13, y=df$end_growth, x=rep.int(2015,length(df)-1),hjust=-.2,size=3)
  p<-p + geom_text(label=l11, y=df$start_growth, x=rep.int(2003,length(df)-1),hjust=1.2,size=3)
  p<-p + geom_text(label="2003", x=2003, y=(1.1*(max(df$start_growth,df$start_growth))),hjust= 1.2,size=4)
  p<-p + geom_text(label="2015", x=2015,y=(1.1*(max(df$end_growth,df$end_growth))),hjust=-0.1,size=4)
  p
}

create_slope_graph(louis_est[1:5,])

df = subset(final_cbp, MSA == 31140 & Year ==2003)
df = select(df, MSA, NAICS)
x0 = subset(final_cbp, MSA ==31140 & Year == 2003)$EMP
x1 = subset(final_cbp, MSA == 31140 & Year == 2015)$EMP
pct_growth = (x1-x0)/x0 *100
df$pct_growth = pct_growth
View(df)

#Given a city

#Funciton calculates the growth rates for all industries starting
#with the first, non-zero data point, if the 0 is due to a 
#flagging issue. 


#Censored Data Problem
y = final_cbp
y = y%>%
  subset(NAICS == "21----" & MSA == 31140)%>%
  select(EMP, MSA, NAICS, Year, EMPFLAG)
View(y)

num_censored_p = final_cbp
num_censored_p = num_censored_p%>%
  subset(MSA == 31140 & (EMPFLAG == ""|NAICS == "------") & Year == 2003)
num_censored = num_censored_p[num_censored_p$NAICS == "------",]$EMP - sum(num_censored_p[num_censored_p$NAICS!="------",]$EMP)

p = final_cbp
p = p%>%
  subset(MSA == 31140 & (EMPFLAG != ""|NAICS == "------") & Year == 2003)
View(p)

NAICS_11_Lou = final_cbp
NAICS_11_Lou = NAICS_11_Lou%>%
  subset(NAICS == "11----" & MSA == 31140)%>%
  select(EMP, MSA, NAICS, Year, EMPFLAG)
View(NAICS_11_Lou)

#Stacked Area Chart
prop_fun<-function(v){
  as.numeric(v[3])/sum(x$EMP[x$Year == v[4]])*100
}

x = final_cbp%>%
  subset(NAICS!="------" & MSA == 31140)%>%
  select(NAICS, EMP, EST, Year)

x$prop_emp =apply(x, 1, prop_fun)

g<-ggplot(x, aes(x = Year, y = EMP, fill =NAICS))
g<-g + geom_area(colour = "black", size = .25)
g





#Industry over Time Draft
x = subset(final_cbp, NAICS == "------" & MSA == 31140 & Year>=2007)
tech = subset(final_cbp, NAICS == "54----" & MSA == 31140 & Year>=2007)

x = select(x, c(MSA, NAICS, EMP, EST, Year))
tech = select(tech, c(MSA, NAICS, EMP, EST, Year))

tech$pct_tech_emp=tech$EMP/x$EMP
tech = mutate(tech, roll_mean_pct_tech_emp = rollmean(pct_tech_emp, 3, fill=list(NA, NULL, NA)))
tech$pct_tech_est=tech$EST/x$EST
tech = mutate(tech, emp_est_ratio = pct_tech_emp/pct_tech_est)

ggplot(tech, aes(x = Year, y = roll_mean_pct_tech_emp))+
  geom_point(shape = 1)+
  geom_smooth(method = "loess")


####################
# Data 2003-2015#
####################

setwd("~/Desktop/GLP/R Code/Emp Data")
bds_data = read.csv("bds_f_msa_release.csv")
# bds_data = rename(bds_data, MSA = msa)

bds_data = bds_data %>%
  rename(MSA = msa, year = year2)%>%
  pull_peers_MSA
bds_data = bds_data[bds_data$year >= 2003,]
bds_data$city<-NA
bds_data$city[bds_data$MSA == 24340] = "Grand Rapids"
bds_data$city[bds_data$MSA == 41180] = "St. Louis"
bds_data$city[bds_data$MSA == 36420] = "Oklahoma City"
bds_data$city[bds_data$MSA == 46140] = "Tulsa"
bds_data$city[bds_data$MSA == 24860] = "Greenville"
bds_data$city[bds_data$MSA == 28940] = "Knoxville"
bds_data$city[bds_data$MSA == 13820] = "Birmingham"
bds_data$city[bds_data$MSA == 31140] = "Louisville"
bds_data$city[bds_data$MSA == 26900] = "Indianapolis"
bds_data$city[bds_data$MSA == 28140] = "Kansas City"
bds_data$city[bds_data$MSA == 36540] = "Omaha"
bds_data$city[bds_data$MSA == 24660] = "Greensboro"
bds_data$city[bds_data$MSA == 16740] = "Charlotte"
bds_data$city[bds_data$MSA == 18140] = "Columbus"
bds_data$city[bds_data$MSA == 17140] = "Cincinnati"
bds_data$city[bds_data$MSA == 34980] = "Nashville"
bds_data$city[bds_data$MSA == 32820] = "Memphis"
bds_data$city[bds_data$MSA == 27260] = "Jacksonville"
bds_data$city[bds_data$MSA == 39580] = "Raleigh"
bds_data$city[bds_data$MSA == 19380] = "Dayton"
bds_data$city[bds_data$MSA == 40060] = "Richmond"

bds_data<-mutate(bds_data, estabs_entry_rate_minus_exit = estabs_entry_rate -estabs_exit_rate)

write.csv(bds_data, file = "GLP_BDS_data.csv", row.names = FALSE)


##rolling mean functions for trendlines
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}
rollmean5 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1] <- NA
    y[2] <- NA
  }
  y
}


graph_trendline_msa<-function(df,var, plot_title="",y_title="Percent", peers = "Current", 
                          caption_text = "", subtitle_text = "", rollmean = 3,
                          break_settings = seq(2005, 2015, 2), xmin = 1996, xmax = 2016){
  df$var <- df[[var]]
  df = df %>% filter(year != 2016)
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & MSA!=31140)
  }
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & MSA!=31140)
  }
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(MSA == 31140) %>% 
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter(year > 2003 & year < 2014)
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter(year > 2006 & year < 2014)
  }
  dat
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value)+min(data_long$value))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value) - border_space, max(data_long$value + border_space)))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p<-p+scale_colour_manual(values=cPalette,
                           labels=c("Louisville", "25th Percentile", "Peer City Mean", "75th Percentile"))+
    scale_linetype_manual(values=c("solid","dashed","dashed","dashed"),
                          labels=c("Louisville", "25th Percentile", "Peer City Mean", "75th Percentile"))
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}



rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="", y_title = "Percent", caption_text = ""){
  df$var <- df[[var]]
  if(peers=="Current"){
    df<-subset(df,current ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline ==1)
  }
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)

  breaks <- classIntervals(d.graph$var,3,style="jenks")
  breaks
  d.graph$color <- NA
  d.graph$color[d.graph$var<=breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var>breaks$brks[3]] <- "red"
  d.graph
  d.graph$round <- format(round(d.graph$var,1),nsmall=1)
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans 300 Italic"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"


  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                               y=var,fill=factor(color)))+guides(fill=FALSE)
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 18, hjust = 0.5),
                 axis.text.y=element_text(hjust=0, family = rev(d.graph$textfont),
                                          size=12, color = rev(d.graph$textcolor)),
                 axis.ticks=element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5))
  p <- p+geom_text(aes(label=round),hjust=1.1, size=5, family = "Museo Sans 300")
  p <- p+labs(title = plot_title, y= y_title,
              x = "", caption = caption_text)
  p
}


font.add("Museo Sans 300", "/Users/BryceRowland/Desktop/GLP/R Code/MuseoSans_300.otf")
font.add("Museo Sans 300 Italic", "/Users/BryceRowland/Desktop/GLP/R Code/MuseoSans_300_Italic.otf")

setwd("~/Desktop/GLP/R Code/Emp Data/Images")

#Create Trendlines
jpeg("jobs_estabs_entry_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'estabs_entry_rate',
                       plot_title = "Establishment Entry Rate",
                       subtitle = "3-year rolling average",
                       rollmean = 3,
                       caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                       break_settings = seq(2004,2014, 2),
                       xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_job_creation_rate_births_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'job_creation_rate_births',
                    plot_title = "Job Creation Birth Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_job_destruction_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'job_destruction_rate',
                    plot_title = "Job Destruction Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_job_destruction_rate_deaths_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'job_destruction_rate_deaths',
                    plot_title = "Job Destruction Death Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_estabs_exit_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'estabs_exit_rate',
                    plot_title = "Establishment Exit Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_job_creation_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'job_creation_rate',
                    plot_title = "Job Creation Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_net_est_entry_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'estabs_entry_rate_minus_exit',
                    plot_title = "Net Establishment Entry Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()

jpeg("jobs_net_job_creation_rate_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_msa(bds_data, 'net_job_creation_rate',
                    plot_title = "Net Job Creation Rate",
                    subtitle = "3-year rolling average",
                    rollmean = 3,
                    caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS",
                    break_settings = seq(2004,2014, 2),
                    xmin = 2004, xmax = 2013)
showtext.end()
dev.off()


#Peer City Comparisons 
jpeg("jobs_estabs_entry_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'estabs_entry_rate',
                  order = "Descending",
                  plot_title = "Establishment Entry Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_estabs_exit_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'estabs_exit_rate',
                  order = "Ascending",
                  plot_title = "Establishment Exit Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_job_creation_rate_births_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'job_creation_rate_births',
                  order = "Descending",
                  plot_title = "Jobs Creation Birth Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_job_creation_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'job_creation_rate',
                  order = "Descending",
                  plot_title = "Job Creation Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_job_destruction_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'job_destruction_rate',
                  order = "Ascending",
                  plot_title = "Job Destruction Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_job_destruction_rate_deaths_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'job_destruction_rate_deaths',
                  order = "Ascending",
                  plot_title = "Job Destruction Death Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_job_creation_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'job_creation_rate',
                  order = "Descending",
                  plot_title = "Job Creation Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_net_est_entry_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'estabs_entry_rate_minus_exit',
                  order = "Descending",
                  plot_title = "Net Establishment Entry Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()

jpeg("jobs_net_job_creation_rate_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(filter(bds_data, year == 2014), 'net_job_creation_rate',
                  order = "Descending",
                  plot_title = "Net Job Creation Rate, 2014",
                  caption_text = "Source: Greater Louisville Project \nData from United States Census Bureau, BDS")
showtext.end()
dev.off()



# emp_data_format<-function(data){
#   data = data %>%
#     rename(MSA = msa, NAICS = naics, 
#            EMPFLAG = empflag, EMP = emp,
#            QP1 = qp1, AP = ap, EST = est,
#            N1_4 = n1_4, N5_9 = n5_9, N10_19 = n10_19,
#            N20_49 = n20_49, N50_99 = n50_99, N100_249 = n100_249,
#            N250_499 = n250_499, N500_999 = n500_999,
#            N1000 = n1000, N1000_1 = n1000_1, N1000_2 = n1000_2,
#            N1000_3 = n1000_3, N1000_4 = n1000_4) %>%
#     select(MSA, NAICS, EMPFLAG, EMP, QP1, AP, EST, N1_4,N5_9,
#            N10_19, N20_49, N50_99, N100_249, N250_499, N500_999,
#            N1000, N1000_1, N1000_2, N1000_3, N1000_4, Year)
#   
#   data
# }  



# econ_data_subset<-function(data){
#   econ_sub = pull_peers_MSA(data)
#   econ_sub = subset(econ_sub,econ_sub$NAICS == "11----"| econ_sub$NAICS == "21----"
#                     |econ_sub$NAICS == "22----" | econ_sub$NAICS == "23----"
#                     |econ_sub$NAICS == "31----" | econ_sub$NAICS == "32----"
#                     |econ_sub$NAICS == "33----" | econ_sub$NAICS == "42----"
#                     |econ_sub$NAICS == "44----" | econ_sub$NAICS == "45----"
#                     |econ_sub$NAICS == "48----" | econ_sub$NAICS == "49----"
#                     |econ_sub$NAICS == "51----" | econ_sub$NAICS == "52----"
#                     |econ_sub$NAICS == "53----" |econ_sub$NAICS == "54----"
#                     |econ_sub$NAICS == "55----" |econ_sub$NAICS == "56----"
#                     |econ_sub$NAICS == "61----" |econ_sub$NAICS == "62----"
#                     |econ_sub$NAICS == "71----" |econ_sub$NAICS == "72----"
#                     |econ_sub$NAICS == "81----" |econ_sub$NAICS == "92----"
#                     | econ_sub$NAICS == "------")
#   econ_sub
# }



# cbp_03 = emp_data_format(cbp_03)
# cbp_04 = emp_data_format(cbp_04)
# cbp_05 = emp_data_format(cbp_05)
# cbp_06 = emp_data_format(cbp_06)
# cbp_07 = emp_data_format(cbp_07)
# cbp_08 = emp_data_format(cbp_08)
# cbp_09 = emp_data_format(cbp_09)
# cbp_10 = emp_data_format(cbp_10)
# cbp_11 = emp_data_format(cbp_11)
# cbp_12 = emp_data_format(cbp_12)
# cbp_13 = emp_data_format(cbp_13)
# cbp_14 = emp_data_format(cbp_14)
# cbp_15 = select(cbp_15, MSA, NAICS, EMPFLAG, EMP, QP1, AP, EST, N1_4,N5_9,
#                 N10_19, N20_49, N50_99, N100_249, N250_499, N500_999,
#                 N1000, N1000_1, N1000_2, N1000_3, N1000_4, Year)



# agg_data<-bds_data%>%
#   subset(fage4==bds_data[1,3])%>%
#   select(Year, MSA, Firms, Estabs, Emp, Denom)
# View(agg_data)
# 
# #Aggregating Employment Data
# aggregate_bds<-function(df){
#   agg_data<-df%>%
#     subset(fage4==df[1,3])%>%
#     select(Year, MSA, Firms, Estabs, Emp, Denom, Estabs_Entry)
#   dis_year_msa<-distinct(df, MSA, Year)
#   for(i in length(c(1:nrow(dis_year_msa)))){
#     my_msa<-dis_year_msa[i,]$MSA
#     my_year<-dis_year_msa[i,]$Year
#     sub_df<-filter(df, Year ==my_year, MSA==my_msa)
#     agg_data$Firms[i,] = sum(sub_df$Firms)
#     agg_data$Estabs[i,] = sum(sub_df$Estabs)
#     agg_data$Emp[i,] = sum(sub_df$Emp)
#     agg_data$Emp[i,] = sum(sub_df$Denom)
#     
#   }
# }
