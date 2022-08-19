library(ggplot2)
library(dplyr)
library(corrplot)
library(assertive)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
credits = read.csv("./credits.csv")
head(credits)
titles = read.csv("./titles.csv")
length(titles)

for (i in colnames(titles)){
  if(sum(is.na(titles[[i]])) > 1)
  cat(i,"->",sum(is.na(titles[[i]])),"\n")
}

length(titles$type[titles$type == "MOVIE"])


# CREDITS
str(credits)
str(titles)
unique(credits$role) # 2 Levels actor and director

summary(credits)
summary(titles)


pysch::describe(credits)
psych::describe(titles)

sum(duplicated(credits,by=c('id','person_id'))) # uniques in credits

hist(titles$release_year)
install.packages('ggrepel')

###############################################################################################
ggplot(titles %>% group_by(release_year) %>% mutate(count = n()), aes(x=release_year, y=count)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9, linetype=1) +
  labs(x='Released year',y='Number of movies during years')+
  theme(panel.background = element_rect(fill='#f8f8f8'),panel.grid = element_blank()) +
  ggtitle("Movie Trend over the Decades ")

movies <- titles %>% filter(type=='MOVIE') %>%
  mutate(Decade = if_else(release_year >= 2000,
                          paste0(release_year  %/% 10 * 10, "'s"),
                          paste0((release_year - 1900) %/% 10 * 10, "'s")))

movies %>% group_by(Decade) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

ggplot(movies %>% group_by(Decade) %>%
         summarise(count=n()) %>% filter(count > 200) %>%
         arrange(desc(count)),aes(y=reorder(Decade, count),x=count)) +
  geom_bar(stat = 'identity',width = 0.2,fill=c('lightblue','blue')) + 
  theme(panel.background = element_rect("#fafafa"),panel.grid = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Movie Production Frequency of Two Recent Decades") + 
  ylab('2 Recent Decades') +
  xlab("") +
  geom_text(aes(label=count), vjust=0.4,hjust=1.5, color="white", size=3.5)

############################################################################

Mode <- function(x){
  ux <- unique(x)
  ux = ux[ux != ""]
  ux = ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))] 
}

# REPLACED age_certification values with MODE of it 
# Actual mode is NA because many samples had NA so replace with second MODE value after that.
titles$age_certification[titles$age_certification == ""] = Mode(titles$age_certification)
# replaced mean in place of NA
titles$imdb_score[is.na(titles$imdb_score)] = mean(titles$imdb_score,na.rm=TRUE)
titles$tmdb_score[is.na(titles$tmdb_score)] = mean(titles$tmdb_score,na.rm=TRUE)

titles <- titles %>%
  mutate(prd_country_fct = as.factor(production_countries))
assert_is_factor(titles$prd_country_fct)

###
movies= movies %>%
  mutate(Duration = ifelse(runtime <=90, "Short", ifelse(runtime <= 120, "Medium", "Long")))
titles= titles %>%
  mutate(Duration = ifelse(runtime <=90, "Short", ifelse(runtime <= 120, "Medium", "Long")))

ggplot(data= movies %>%
        group_by(Duration) %>% 
         filter(type=='MOVIE') %>%
        summarise(count=n()), aes(y=count, x=Duration, fill=count)) +
  geom_bar(stat= 'identity',width=0.6)+
  xlab('Duration of the Movies') +
  ylab("Number of Movies")  +
  ggtitle("Production vs Runtime of Movies")+
  theme(panel.background = element_rect(fill='#ffffff'),legend.position = 'none',axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip() +
  geom_text(aes(label=(count)), vjust=0.4,hjust=1.5, color="white", size=3.5)
  
##
movies <- movies %>%
  mutate(Type_Of_Movie = ifelse(release_year <=1953, "Old", ifelse(release_year <= 1990, "Classic", "Modern")))

ggplot(movies %>%
         group_by(Type_Of_Movie, Duration) %>%
         summarise(count=n()), aes(y=count, x=Type_Of_Movie, fill=Duration)) +
  geom_bar(stat='identity') + theme(panel.background = element_rect('white')) +
  ggtitle("Frequency vs Type vs Runtime of Movies")

#####

titles <- titles %>% mutate(voted = ifelse(imdb_votes <=1000, "Less", ifelse(imdb_votes <= mean(titles$imdb_votes,na.rm = T), "Average", "Highly")))
ggplot(titles %>% filter(!is.na(voted)),aes(x=tmdb_score,y=imdb_score)) +
  geom_count(alpha=0.25)+ coord_flip()+ stat_smooth(method='lm')+
  facet_grid(rows=vars(type),cols=vars(voted)) +
  #ggtitle('TMDB and IMDB Rating vs IMDB Votes vs Type of entertainment') + 
  theme(panel.background = element_rect(fill='#fcfcfc')) +
  xlab('IMDB Rating') + ylab("TMDB Rating")

##
titles <- titles %>%
  mutate(voted = ifelse(imdb_votes <=1000, "Very Few", ifelse(imdb_votes <= mean(movies$imdb_votes,na.rm = T), "Average", "Highly")))

ggplot(title %>% filter(!is.na(voted)),aes(tmdb_score,imdb_score,fill=voted,color=voted)) +
  geom_point(alpha=0.25,col='red') + 
  geom_count()+
  coord_flip()+
  stat_smooth(method='lm')+
  facet_grid(. ~ voted)
##
titles <- titles %>%
  mutate(Decade = if_else(release_year >= 2000,
                          paste0(release_year  %/% 10 * 10, "'s"),
                          paste0((release_year - 1900) %/% 10 * 10, "'s")))
titles %>%
  group_by(Decade) %>%
  summarise(count=n())
show_only = titles %>% filter(!is.na(voted))
ggplot(show_only,aes(Duration,imdb_score,col=Duration)) +
  geom_point(position = position_fill())+
  facet_grid(. ~ type) + 
  stat_summary(fun.data = mean_sdl,fun.args = list(mult = 1),
               position = position_dodge(width=0.3),geom = 'errorbar')
##

titles = titles %>% mutate(prd_country = if_else(grepl("'US'",titles$production_countries,fixed=T),'US',
                                                 if_else(grepl("JP",titles$production_countries,fixed=T),'Japan', 
                                                  ifelse(grepl("GB",titles$production_countries,fixed=T),'Great Britain',
                                                  ifelse(grepl("CA",titles$production_countries,fixed=T),'Canada','Other')
                                                 ))))
ggplot(titles,aes(Decade,prd_country,col=type)) +
  geom_count(position = 'identity')+
  facet_wrap(Duration ~ type ) +
  coord_flip() +
  xlab('Decades') + 
  ylab('Countries of Production') + 
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),panel.background = element_rect(fill="#fafafa"))


###

titles = titles  %>% mutate(cat_genres = if_else(grepl("'comedy'",titles$genres,fixed=T),'Comedy',
                                                 if_else(grepl("action",titles$genres,fixed=T),'Action', 
                                                         ifelse(grepl("Crime",titles$genres,fixed=T),'Crime',
                                                                ifelse(grepl("documentation",titles$genres,fixed=T),'Documentation',ifelse(grepl("scifi",titles$genres,fixed=T),'scifi',ifelse(grepl("thriller",titles$genres,fixed=T),'Thriller','Other')))
                                                         )))) %>% filter(!is.na(voted)) %>% arrange(desc(voted))
ggplot(titles,aes(cat_genres,voted,col=Duration)) +
  geom_count(position = 'identity')+
  facet_wrap(Duration ~ type ) +
  coord_flip() +
  xlab('Genres') + 
  ggtitle('Genre vs Numbers of vote vs Type of Movies') +
  ylab('Countries of Production') + 
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),panel.background = element_rect(fill="#fafafa"))

movies$imdb_score[is.na(movies$imdb_score)] = mean(movies$imdb_score,na.rm=TRUE)
movies= movies %>% 
  mutate(overall_rating = ifelse(imdb_score <=2.5, "Poor", ifelse(imdb_score <= mean(movies$imdb_score,na.rm = TRUE), "Below Avg. or Avg.", ifelse(imdb_score <=6.5,'Good',"Excellent"))))

movies= movies %>% 
  mutate(tmdb_overall_rating = ifelse(tmdb_score <=2.5, "Poor", ifelse(tmdb_score <= mean(movies$tmdb_score,na.rm = TRUE), "Below Avg. or Avg.", ifelse(tmdb_score <=6.5,'Good',"Excellent"))))

ggplot(movies,aes(runtime,y=overall_rating,fill=tmdb_overall_rating)) +
  geom_point(position = position_jitter(width = 0.2)) +
  facet_grid(. ~ tmdb_overall_rating) +
  coord_flip() +
  xlab('Runtime') + 
  ylab('IMDB Score') +
  stat_summary(fun = mean_sdl,position = position_dodge(width = 1)) +
  theme(axis.text.x = element_text(angle=c(45),vjust =1,hjust = 1))



#CREDIT 
ggplot(credits %>% group_by(role) %>% summarise(count = (n()/nrow(credits))*100) 
    ,aes(y=count,x=unique(role))) +
  geom_bar(stat='identity',width = 0.2,fill='#FC8E62') +
  geom_text(aes(label=round(count)),vjust=1.5,col='white') +
  labs(x='Role in Credits',y='R.Frequency') +
  ggtitle("Relative frequency of Roles in Credits") +
  theme(panel.background = element_rect(fill="#fafafa"))

movies = titles %>% filter(type == "MOVIE")
SHOWs = titles %>% filter(type == 'SHOW')
SHOWs = select(SHOWs,-c(id,title,description,type,age_certification,imdb_id,Duration,prd_country_fct))
SHOWs = select(SHOWs,-c(voted,Decade,cat_genres,prd_country))
SHOWs = select(SHOWs,-c(id,title,description,type,age_certification,imdb_id,Duration,prd_country_fct))
SHOWs = select(SHOWs,-c(voted,Decade,cat_genres,prd_country))
SHOWs$genres = as.numeric(as.factor(SHOWs$genres))
SHOWs$production_countries = as.numeric(as.factor(SHOWs$production_countries))
corrplot(cor(SHOWs,use='na.or.complete'))

#for movies
movies = titles %>% filter(type == "MOVIE") %>% select(-c(id,title,description,type,age_certification,imdb_id,'seasons',Duration,prd_country_fct))

#movies = select(movies,-c(id,title,description,type,age_certification,imdb_id))
movies$genres = as.numeric(as.factor(movies$genres))
movies$production_countries = as.numeric(as.factor(movies$production_countries))
corrplot(cor(movies,use='na.or.complete'))

ggplot(SHOWs,aes(x=seasons,y=Decade,co)) + geom_bar(stat='identity')



