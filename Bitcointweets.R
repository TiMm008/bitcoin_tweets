# Import libraries
library(rtweet)
library(ggplot2)
library(data.table)
library(magrittr)
library(quanteda)
library(quanteda.textstats) # loads function textstat_frequency to name space

#Save and load
# Assembling data
save(tweets.df, file ="bittweets.RData")
load(file ="bittweets.RData")

# Import tweets
tweets.df = search_tweets(
  "bitcoin",
  n = 18000,
  type = "recent",
  include_rts = FALSE, #No retweets, only original tweets!
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = NULL,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en",
  since = "2021-12-01",
  until = "2021-12-03",
  tweet_mode = "extended" # get 240 character tweets in full
)

# Set as a data table
setDT(tweets.df)

#Dimensions
dim(tweets.df)

#Highest number of retweets
tweets.df[retweet_count>1000, retweet_count+favorite_count]

# Tweets and reactions by country
tweets.df[,.(TotalTweets = .N, 
             total_reactions=sum(retweet_count, na.rm = TRUE) + 
               sum(favorite_count, na.rm = TRUE)+
               sum(reply_count, na.rm = TRUE)+
               sum(quote_count, na.rm = TRUE)), 
          by = .(country)] [order(-total_reactions)]


# Tweets and reactions by User_id
tweets.df[,.(TotalTweets = .N, 
             total_reactions=sum(retweet_count, na.rm = TRUE) + 
               sum(favorite_count, na.rm = TRUE)+
               sum(reply_count, na.rm = TRUE)+
               sum(quote_count, na.rm = TRUE)), 
          by = .(screen_name)] [order(-total_reactions)]

# Most common hashtags
tweets.df$hashtags %>%           #the column of hashtags
  unlist %>%                    #if a single tweet has several hashtags, unlist them 
  table %>%                     # make a frequency table 
  sort(decreasing = TRUE) %>%   # sort it by decreasing frequency
  head(15)                      # display the 15 most common

# Most shared links
tweets.df$urls_expanded_url %>%  #the column of url for linked content
  unlist %>%           #if a tweet has several urls, unlist them 
  table %>%            # make a frequency table 
  sort(decreasing = TRUE) %>% # sort it by decreasing frequency
  head(5)            # display the 5 most common

#Plot with number of tweets
ggplot(tweets.df, aes(x=created_at, y=(sum("user_id")))) +
  geom_histogram(aes(y=..count..), binwidth=500, 
                 colour = "black", fill = "cyan") +
  scale_x_datetime(name = "Time") +
  scale_y_log10(name = "Number of tweets", breaks = c(10,100,1000,10000) ) +
  theme_minimal()

#Plot with number of tweets per minute
ggplot(tweets.df, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), #make histogram
                 binwidth=60, #each bar contains number of tweets during 60 s
                 colour="blue", #colour of frame of bars
                 fill="blue", #fill colour for bars
                 alpha=0.8) + # bars are semi transparant
  ggtitle(paste0("Activity ",dim(tweets.df)[1]," tweets")) + #title
  scale_y_continuous(name="Number of Tweets per minute") + 
  scale_x_datetime(name = "Time") 

#Number of tweets per friend count - potential reach
ggplot(tweets.df, aes(
  x=created_at, 
  y=(friends_count+1), 
  size = favorite_count + reply_count + quote_count + retweet_count )
) +
  geom_point(aes(size = retweet_count), alpha = 0.5) +
  ggtitle(paste0("Each dot is a tweet matching 'Dune'")) +
  scale_y_log10(name="Potential Reach",breaks = c(10,100,1000,10000) ) +
  scale_x_datetime(name = "Time") +
  scale_size_continuous(name="Retweets") +
  theme_minimal()

# Tokenize tweets
tok_tweets = tweets.df$text %>% 
  gsub("#","", . ) %>% 
  corpus %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)
head(tok_tweets,n=2)

#Stopwords
stopwords(language = "en")[1:10]
tok_tweets = tokens_remove(tok_tweets,stopwords(language = "en"))
head(tok_tweets,n=2)

# Word frequency

words.to.remove = c(stopwords("english"),'Bitcoin',"Bitcoins","bitcoin","#")
dfmat_corp_twitter = tweets.df$text %>%
  gsub("#","", . ) %>%
  corpus %>%
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE) %>%
  tokens_remove(words.to.remove) %>%
  tokens_wordstem(language = "en") %>%
  dfm()

dfFreq = textstat_frequency(dfmat_corp_twitter) %>%
  as.data.table
dfFreq_long_top20 = dfFreq[rank <= 20] %>%
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )
ggplot(dfFreq_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() +
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()

# Word 2-gram
require(quanteda.textstats)
TokensStemmed = tokens_remove(tok_tweets, words.to.remove)

dfm2 = dfm(tokens_ngrams(TokensStemmed,n=2))

dfFreq2 = textstat_frequency(dfm2)

ggplot(dfFreq2[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "2 gram") +
  theme(text=element_text(size=12))

fstat = dfmat_corp_twitter %>% 
  dfm_trim(min_termfreq = 0.993, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")
plot(hclust(as.dist(fstat)))


# Sentiment analyisis
require(sentimentr)
tweets.df$text[1]
tweets.df$text[1] %>% get_sentences
tweets.df$text[1] %>% get_sentences %>% sentiment
tweets.df$text[1] %>% get_sentences %>% sentiment_by
sentiment_by_tweet = 
  tweets.df$text %>% get_sentences %>% sentiment_by()


# make sure that the columns of sentiment_by_tweet have not already been added to tweets.df
tweets.df[,colnames(sentiment_by_tweet) :=NULL]
# then add them to tweets.df
tweets.df = cbind(tweets.df,sentiment_by_tweet)

# 10 most negatively charged tweets
tweets.df[,.(text, ave_sentiment)][order(-ave_sentiment)] %>% tail(10)

# 10 most positively charged tweets
tweets.df[,.(text, ave_sentiment)][order(-ave_sentiment)] %>% head(10)
# Average sentiment overall:
hist(tweets.df$ave_sentiment)
mean(tweets.df$ave_sentiment)

# The 10 most frequent negative terms, regardless of topic
require(data.table)
require(magrittr)
require(sentimentr)

tweets.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# The 10 most frequent positive terms:
tweets.df[,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# Topic modelling
require(topicmodels)
dtm = convert(dfmat_corp_twitter, to = "topicmodels")
lda = LDA(dtm, k = 5, control=list(seed=12))
terms(lda, 12)


# Network of users
if (!require(vosonSML)) {install.packages('vosonSML'); require(vosonSML)}
class(tweets.df)
class(tweets.df) <- c(class(tweets.df),"datasource","twitter")
class(tweets.df)

## Bitcoin network - nodes are users who have tweeted
actorGraph <- tweets.df[] %>%      # tweets data table
  Create("actor") %>%             # Actor graph 
  Graph()                         # igraph network graph
source("graphHelpFunctions.R")
actorGraph.simplyfied = simplify.actor.network(actorGraph, remove.loops = TRUE, delete.zero.degree = TRUE)
grep("^layout_with_.*[^[sugiyama]]*", ls("package:igraph"), value = TRUE) %>%  print
plot.actor.Graph(actorGraph.simplyfied, 
                 vertex.label = NA, 
                 layout = layout_with_drl)


layout(matrix(1:2,1,2,byrow = TRUE), widths = c(5,2))
plot.igraph(actorGraph.simplyfied, 
            layout = layout_with_drl,
            asp =0,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0.15           ## 0 to 1 or TRUE (0.5)
) 

plot.new()
legend(x = "left",      ## position, also takes x,y coordinates
       legend = named.users,
       pch = 19,              ## legend symbols see ?points
       col = color_vector,
       bty = "n",
       cex=0.5, #font size 50%
       title = "Users")

top.ranked.users(actorGraph.simplyfied)[1:5]
named.users = top.ranked.users(actorGraph.simplyfied)[1:5]


#if I only want to concentrate on the network around the user "airdrop"

actorGraph %>%  
  neighborhood.to.user("AirdropStario",  k.nearest.neighbours=1) %>% 
  plot.actor.Graph(vertex.label = NA, layout = layout_with_kk)

actorGraph %>% 
  simplify.actor.network %>%  
  top.ranked.users() %>% 
  head(5)
actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("AirdropStario",  k.nearest.neighbours=2) %>% 
  plot.actor.Graph(vertex.label = NA, layout = layout_with_drl)


ranked.users = actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("AirdropStario",  k.nearest.neighbours=2) %>% 
  top.ranked.users() %>% head(8)

actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("AirdropStario",  k.nearest.neighbours=2) %>% 
  label.user.network(ranked.users) %>% 
  plot.actor.Graph(layout = layout_with_fr) 
