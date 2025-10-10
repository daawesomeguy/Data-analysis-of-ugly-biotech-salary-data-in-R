library(tidyverse,ggplot2)
data <- readr::read_csv('/Users/jacknugent/Downloads/Copy of r_biotech salary and company survey - 2024.csv')
names(data)=make.names(names(data))
names(data)
#rename(data,'level_education'=Select.the.highest.level.of.education.that.you.have.that.s.relevant.to.your.occupation..If.you.have.multiple..e.g..PhD...MD..please.select..Other..and.describe)
#data = filter(data,What.degrees.do.you.have. == 'PhD or Equivalent')
#data = filter(data,Where.are.you.located.=='New England (MA, CT, RI, NH, VT, ME)' | Where.is.the.closest.major.city.or.hub. == 'Boston')
#data= filter(data,Biotech.sub.industry.=='Industrial Biotech')
top_three_categories <- data |>
  count(Where.are.you.located., sort = TRUE) |> # Count occurrences of each category and sort
  slice_head(n = 2)            # Take the top 3 rows
#  pull(Where.are.you.located.)  
data <- filter(data, Where.are.you.located. %in% top_three_categories$Where.are.you.located.)
data <- filter(data,Years.of.Experience<25)
p<- ggplot(data,aes(x=as.factor(Years.of.Experience),y=Compensation...Annual.Base.Salary.Pay,fill=Where.are.you.located.))
p<- p+geom_boxplot()+coord_cartesian(ylim = c(0, 350000))+scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+geom_smooth(se=TRUE, aes(group=1))
  #  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))
#p<- p+ geom_jitter()+geom_smooth()+scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
#  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))

print(p)
