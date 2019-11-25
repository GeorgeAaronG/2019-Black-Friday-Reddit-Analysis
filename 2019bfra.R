# 2019 Black Friday Reddit Analysis
#
# Social media mining of Reddit for conversations about Black Friday, 2019.  Specifically, what
# can discussions around Black Friday deals tell us about customer engagement over 3 Black Friday
# industry categories: superstores, consumer electronics, and retail.
# 
# I. Choosing posts in the **Black Friday** community
# II. Plotting an interactive conversation network
# III. Processing the conversations with NLP for entity analysis
# IV. Visualizing the entities by type

##################
# Preliminaries

# To prevent java.lang.OutOfMemoryError, set Java heap size
options(java.parameters = "-Xmx8g")

# Libraries
library(RedditExtractoR)
library(dplyr)
library(NLP)
library(openNLP)
library(openNLPmodels.en)


####################
# I. Choosing posts in the **Black Friday** community

reddit_links <- reddit_urls(search_terms = "black friday", subreddit = 'blackfriday')
reddit_thread <- reddit_links$URL

# Check and set links for analytics
analyticsWM <- reddit_content(reddit_thread[1]) # Wal Mart, Nov. 14, 229 comments
analyticsBB <- reddit_content(reddit_thread[3]) # Best Buy, Nov. 7, 173
analyticsK <-  reddit_content(reddit_thread[20]) # Kohls, Nov. 7, 36 comments

analyticsX <- analyticsWM


#######################
# II. Plotting an interactive conversation network

# Create a graph file from single thread
graph_object <- construct_graph(analyticsX)

# Interactive user relationship network
network_list <- analyticsX %>% user_network(include_author=TRUE, agg=TRUE)
network_list$plot


##########################
# III. Processing the conversations with NLP for entity analysis

Reddit <- data.frame(analyticsX$id, analyticsX$comment)
colnames(Reddit) <- c('Post', 'Comment')

person_annotator=Maxent_Entity_Annotator(kind='person')
organization_annotator=Maxent_Entity_Annotator(kind='organization')
location_annotator=Maxent_Entity_Annotator(kind='location')
date_annotator=Maxent_Entity_Annotator(kind='date')
money_annotator=Maxent_Entity_Annotator(kind='money')
percentage_annotator=Maxent_Entity_Annotator(kind='percentage')

RedditEntities=data.frame(Post=numeric(), Type=character(), Entity=character(), Position=numeric(), stringsAsFactors=FALSE)

for (post in 1:nrow(Reddit))  # repeat for each row in dataframe
{
  RedditText=as.String(Reddit[post,2]) # retrieve text
  RedditTokens=annotate(RedditText, 
                        list(Maxent_Sent_Token_Annotator(), # Sentence token
                             Maxent_Word_Token_Annotator())) # Word token
  RedditPersTokens=annotate(RedditText, list(person_annotator), RedditTokens) # set up annotator for persons
  RedditOrgsTokens=annotate(RedditText, list(organization_annotator), RedditTokens) # set up annotator for organizations
  RedditLocsTokens=annotate(RedditText, list(location_annotator), RedditTokens) # set up annotator for locations
  RedditDatsTokens=annotate(RedditText, list(date_annotator), RedditTokens) # set up annotator for dates
  RedditMonsTokens=annotate(RedditText, list(money_annotator), RedditTokens) # set up annotator for monies
  RedditPctsTokens=annotate(RedditText, list(percentage_annotator), RedditTokens) # set up annotator for percentages
  
  RedditPerson=subset(RedditPersTokens,RedditPersTokens$features=='list(kind = "person")') # extract persons
  RedditOrganization=subset(RedditOrgsTokens,RedditOrgsTokens$features=='list(kind = "organization")') # extract organizations
  RedditLocation=subset(RedditLocsTokens,RedditLocsTokens$features=='list(kind = "location")') # extract locations
  RedditDate=subset(RedditDatsTokens,RedditDatsTokens$features=='list(kind = "date")') # extract dates
  RedditMoney=subset(RedditMonsTokens,RedditMonsTokens$features=='list(kind = "money")') # extract monies
  RedditPercentage=subset(RedditPctsTokens,RedditPctsTokens$features=='list(kind = "percentage")') # extract percentages
  
  # Add extracted persons to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditPerson))) # repeat for each row in the persons list
  {
    if (nrow(as.data.frame(RedditPerson))>0) {
      # add post ID, 'Person', name of person extracted, and start position in text into empty RedditEntries dataframe
      RedditEntities=rbind(RedditEntities, cbind(post, 'Person', substr(paste(RedditText, collapse=' '),
                                                                        RedditPerson$start[i],RedditPerson$end[i]), # These extract entire character field from start position to finish position
                                                 RedditPerson$start[i])) 
    }
  }
  
  # Add extracted organizations to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditOrganization)))
  {
    if (nrow(as.data.frame(RedditOrganization))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Organization', substr(paste(RedditText, collapse=' '),
                                                                              RedditOrganization$start[i],RedditOrganization$end[i]),RedditOrganization$start[i]))
    }
  }
  
  # Add extracted locations to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditLocation)))
  {
    if (nrow(as.data.frame(RedditLocation))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Location', substr(paste(RedditText, collapse=' '),
                                                                          RedditLocation$start[i],RedditLocation$end[i]),RedditLocation$start[i]))
    }
  }
  
  # Add extracted dates to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditDate)))
  {
    if (nrow(as.data.frame(RedditDate))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Date', substr(paste(RedditText, collapse=' '),
                                                                      RedditDate$start[i],RedditDate$end[i]),RedditDate$start[i]))
    }
  }
  
  # Add extracted monies to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditMoney)))
  {
    if (nrow(as.data.frame(RedditMoney))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Money', substr(paste(RedditText, collapse=' '),
                                                                       RedditMoney$start[i],RedditMoney$end[i]),RedditMoney$start[i]))
    }
  }
  
  # Add extracted percentages to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditPercentage)))
  {
    if (nrow(as.data.frame(RedditPercentage))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Percentage', substr(paste(RedditText, collapse=' '),
                                                                            RedditPercentage$start[i],RedditPercentage$end[i]),RedditPercentage$start[i]))
    }
  }
}

colnames(RedditEntities)=c('Post', 'Type', 'Entity', 'Position')
RedditExtratedEntities=merge(RedditEntities, Reddit, by.x='Post', by.y='Post')
View(RedditExtratedEntities)

RedditExtratedEntities <- entitiesWM
######################
# IV. Visualizing the entities by type

library(ggpubr)
# Summing the frequencies by entity type and computing proportions
freqTable <- RedditExtratedEntities %>% group_by(Type) %>% summarise(counts = n())
freqTable
freqTrans <- freqTable %>% mutate(prop = round(counts*100/sum(counts), 1),
                                  lab.ypos = cumsum(prop) - 0.5*prop)
freqTrans

# Pie chart
ggpie(freqTrans, x = "prop",
      label = "prop",
      lab.pos = "in",
      lab.font = list(color = "white"),
      fill = "Type",
      color = "white",
      palette = "jco")
