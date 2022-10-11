# loading libraries and EDA #
library(tm)
library(wordcloud)
Reviews <- read.csv("tourist_accommodation_reviews_updated.csv", header = TRUE);

#Inspect the datasets---
names(Reviews)
head(Reviews)
tail(Reviews)
summary(Reviews)
str(Reviews)
dim(Reviews)

# Filter the products from the main dataset and create 5 separate datasets
R_Company<- subset (Reviews, name="The Pizza Company")
R_Side<-subset(Reviews, name="Flip Side")
R_Mar_Phuket<-subset(Reviews, name="Cafe del Mar Phuket")
R_Family_Restaurant<-subset(Reviews, name="H.C. Andersen - Family Restaurant")
R_House<-subset(Reviews, name="Spice_House")

#Inspect the review column in the datasets---
head(R_Company)
head(R_Side)
head(R_Mar_Phuket)
head(R_Family_Restaurant)
head(R_House)

#Create text vectors and cleaning ----
review_Company<-R_Company
review_Side<-R_Side
review_Mar_Phuket<-R_Mar_Phuket
review_Family_Restaurant<-R_Family_Restaurant
review_House<-R_House

#Convert all text to lower case---
review_Company<-tolower(review_Company)
review_Side<-tolower(review_Side)
review_Mar_Phuket<-tolower(review_Mar_Phuket)
review_Family_Restaurant<-tolower(review_Family_Restaurant)
review_House<-tolower(review_House)

#Remove links from the reviews
review_Company <-gsub("http\\S+\\s*", "", review_Company)
review_Side <-gsub("http\\S+\\s*","", review_Side)
review_Mar_Phuket <-gsub("http\\S+\\s*","", review_Mar_Phuket)
review_Family_Restaurant <-gsub("http\\S+\\s*","",review_Family_Restaurant)
review_House <-gsub("http\\S+\\s*","", review_House)

#Remove punctuation from the reviews
review_Company <- gsub("[[:punct:]]", "", review_Company)
review_Side <- gsub("[[:punct:]]", "", review_Side)
review_Mar_Phuket <- gsub("[[:punct:]]", "", review_Mar_Phuket)
review_Family_Restaurant <- gsub("[[:punct:]]", "", review_Family_Restaurant)
review_House <- gsub("[[:punct:]]", "", review_House)

#Remove digits from the reviews
review_Company <- gsub("[[:digit:]]", "", review_Company)
review_Side <- gsub("[[:digit:]]", "", review_Side)
review_Mar_Phuket <- gsub("[[:digit:]]", "", review_Mar_Phuket)
review_Family_Restaurant <- gsub("[[:digit:]]", "", review_Family_Restaurant)
review_House <- gsub("[[:digit:]]", "", review_House)

#Remove leading blank spaces at the beginning from the reviews
review_Company <- gsub("^ ", "", review_Company)
review_Side <- gsub("^ ", "", review_Side)
review_Mar_Phuket <- gsub("^ ", "", review_Mar_Phuket)
review_Family_Restaurant <- gsub("^ ", "", review_Family_Restaurant)
review_House <- gsub("^ ", "", review_House)

#Remove blank spaces at the end from the reviews
review_Company <- gsub(" $", "", review_Company)
review_Side <- gsub(" $", "", review_Side)
review_Mar_Phuket <- gsub(" $", "", review_Mar_Phuket)
review_Family_Restaurant <- gsub(" $", "", review_Family_Restaurant)
review_House <- gsub(" $", "", review_House)

#Remove "tablet" word from the reviews
review_Company <- gsub("tablet", "", review_Company)
review_Side <- gsub("tablet", "", review_Side)
review_Mar_Phuket <- gsub("tablet", "", review_Mar_Phuket)
review_Family_Restaurant <- gsub("tablet", "", review_Family_Restaurant)
review_House <- gsub("tablet", "", review_House)

#Inspect the vectors after cleaning
head(review_Company)
head(review_Side)
head(review_Mar_Phuket)
head(review_Family_Restaurant)
head(review_House)


# Transformation and cleaning up of corpus ----
#Converting the text vectors to corpus
corpus_Company<- Corpus(VectorSource(review_Company))
corpus_Side<- Corpus(VectorSource(review_Side))
corpus_Mar_Phuket<- Corpus(VectorSource(review_Mar_Phuket))
corpus_Family_Restaurant<- Corpus(VectorSource(review_Family_Restaurant))
corpus_House<- Corpus(VectorSource(review_House))


#Clean up corpus by removing stop words and Whitespace
corpus_Company <- tm_map(corpus_Company, removeWords,stopwords("english"))
corpus_Company <- tm_map(corpus_Company, stripWhitespace)
inspect(corpus_Company)

corpus_Side<-tm_map(corpus_Side, removeWords,stopwords("english"))
corpus_Side <- tm_map(corpus_Side, stripWhitespace)
inspect(corpus_Side)

corpus_Mar_Phuket <- tm_map(corpus_Mar_Phuket, removeWords,stopwords("english"))
corpus_Mar_Phuket <- tm_map(corpus_Mar_Phuket, stripWhitespace)
inspect(corpus_Mar_Phuket)

corpus_Family_Restaurant<-tm_map(corpus_Family_Restaurant, removeWords,stopwords("english"))
corpus_Family_Restaurant <- tm_map(corpus_Family_Restaurant, stripWhitespace)
inspect(corpus_Family_Restaurant)

corpus_House <- tm_map(corpus_House, removeWords,stopwords("english"))
corpus_House <- tm_map(corpus_House, stripWhitespace)
inspect(corpus_House)


#Stem the words to their root of all reviews present in the corpus
stem_corpus_Company <- tm_map(corpus_Company, stemDocument)
stem_corpus_Side <- tm_map(corpus_Side, stemDocument)
stem_corpus_Mar_Phuket <- tm_map(corpus_Mar_Phuket, stemDocument)
stem_corpus_Family_Restaurant <- tm_map(corpus_Family_Restaurant, stemDocument)
stem_corpus_House <- tm_map(corpus_House, stemDocument)


#Load the positive and negative lexicon data
positive_lexicon <- read.csv("positive-lexicon.txt")
negative_lexicon <- read.csv("negative-lexicon.txt")

#Inspect lexicons
head(positive_lexicon)
tail(positive_lexicon)

head(negative_lexicon)
tail(negative_lexicon)

sentiment <- function(stem_corpus)
{
  #generate wordclouds
  wordcloud(stem_corpus,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 100)
  #Calculating the count of total positive and negative words in each review

  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus)
  
  for(i in 1:size)
  {
    #All the words in current review
    corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
    
    #positive words in current review
    pos_count <-length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
    
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
    
  }

  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  
  #Create a dataframe with all the positive and negative reviews
  df<-data.frame(Review_Type=c("Postive","Negitive"),
                 Count=c(total_pos_count ,total_neg_count ))
  print(df) #Print
  overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                     round(overall_positive_percentage,2),"%")
  return(overall_positive_percentage)
}

# Use sentiment() function and calculate the Percentage of Positive Reviews
sentiment(stem_corpus_Company)
sentiment(stem_corpus_Side)
sentiment(stem_corpus_Mar_Phuket)
sentiment(stem_corpus_Family_Restaurant)
sentiment(stem_corpus_House)








