#####################################################################
#                                                                   #
# STEP 1: IMPORT ALL 3 DATASETS TO R ENVIRONEMENT                   #
#                                                                   #        
#####################################################################

getwd()
working_dir<-"C:/Users/NAFIS-NEHAN/Documents/csv_tweets1"
setwd(working_dir)

    rahul<-read.csv('Rahul_tweets_small.csv', header=TRUE, sep=",", 
                       check.names=TRUE, stringsAsFactors=FALSE)
    kejriwal <-read.csv('Kejriwal_tweets_small.csv', header=TRUE, sep=",", 
                            check.names=TRUE, stringsAsFactors=FALSE)
    modi <-read.csv('Modi_tweets_small.csv', header=TRUE, sep=",", 
                        check.names=TRUE, stringsAsFactors=FALSE)


#####################################################################
#                                                                   #
# STEP 2: GET SAMPLE OF THESE 3 DATSETS & CLEAN THE DATA            #
#                                                                   #        
#####################################################################

rahul_sample<-rahul[sample(1:nrow(rahul), 3000), ]
    kejriwal_sample<-kejriwal[sample(1:nrow(kejriwal), 3000), ]
        modi_sample<-modi[sample(1:nrow(modi), 3000), ]

##--- Clean all 3 sample datasets with clean.text function

      clean.text <- function(some_txt)
          {  
          
            some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
            some_txt = gsub("@\\w+", "", some_txt)
            some_txt = gsub("[[:punct:]]", "", some_txt)
            some_txt = gsub("[[:digit:]]", "", some_txt)
            some_txt = gsub("http\\w+", "", some_txt)
            some_txt = gsub("[ \t]{2,}", "", some_txt)
            some_txt = gsub("^\\s+|\\s+$", "", some_txt)
            some_txt = gsub("[^\x20-\x7E]", "", some_txt)
            
                  try.tolower = function(x)
                {      
                  y = NA
                  try_error = tryCatch(tolower(x), error=function(e) e)
                  if (!inherits(try_error, "error"))
                    y = tolower(x)
                  return(y)
                  
                }
            some_txt = sapply(some_txt, try.tolower)
            some_txt = some_txt[some_txt != ""]
            names(some_txt) = NULL
            return(some_txt)
          }

rahul_content <- clean.text(rahul_sample$content)
kejriwal_content <- clean.text(kejriwal_sample$content)
modi_content <- clean.text(modi_sample$content)

      rahul_sample <- as.data.frame(rahul_sample)
      rahul_sample$pub_time <- as.Date(rahul_sample$pub_time)
      rahul_sample$content <- rahul_content

      kejriwal_sample <- as.data.frame(kejriwal_sample)
      kejriwal_sample$pub_time <- as.Date(kejriwal_sample$pub_time)
      kejriwal_sample$content <- kejriwal_content

      modi_sample <- as.data.frame(modi_sample)
      modi_sample$pub_time <- as.Date(modi_sample$pub_time)
      modi_sample$content <- modi_content

str(rahul_sample)
str(kejriwal_sample)
str(modi_sample)


#####################################################################
#                                                                   #
# STEP 3: PERFORM SENTIMENT ANALYSIS OF TWEETS                      #
#         CLASSIFIED BY EMOTIONS & POLARITY(Learning Based)         #    
#                                                                   #
#####################################################################

library(sentiment)      # classify by emotion and polarity 
library(ggplot2)        # implemntation of graphics in a small scale

##--- classified all tweets related to Rahul
##--- [Party:Indian National Congress(INC)]
  
    rahul_emo = classify_emotion(rahul_content, algorithm="voter", 
                                 prior=1.0, verbose= FALSE)     
                emotion = rahul_emo[,7]                                         
                emotion[is.na(emotion)] = "confuse"                             
    
    rahul_pol = classify_polarity(rahul_content, algorithm="voter")  
                polarity = rahul_pol[,4]
    
    rahul_sentiment = data.frame(text=rahul_content, emotion=emotion,
                  polarity=polarity, stringsAsFactors=FALSE)
    rahul_sentiment = within(rahul_sentiment,
                  emotion <- factor(emotion, levels=names(sort(table(emotion), 
                             decreasing=TRUE))))
  
    ggplot(rahul_sentiment, aes(x=emotion))+
           geom_bar(aes(y=..count.., fill=emotion))+
        xlab("Classified by Emotions")+ylab("Number of Tweets")+ 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- RAHUL")
    
    ggplot(rahul_sentiment, aes(x=polarity))+
           geom_bar(aes(y=..count.., fill=polarity))+ 
        xlab("Classified by Polarity")+ylab("Number of Tweets")+ 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- RAHUL")
    

##--- classified all tweets related to Kejriwal
##--- [Party:Aam Aadmi Party(AAP)] 

    kejriwal_emo = classify_emotion(kejriwal_content, algorithm="voter", 
                                    prior=1.0, verbose= FALSE)     
                   emotion = kejriwal_emo[,7]                                         
                   emotion[is.na(emotion)] = "confuse"                             
    
    kejriwal_pol = classify_polarity(kejriwal_content, algorithm="voter")  
                   polarity = kejriwal_pol[,4]
    
    kejriwal_sentiment = data.frame(text=kejriwal_content, emotion=emotion,
                                    polarity=polarity, stringsAsFactors=FALSE)
    kejriwal_sentiment = within(kejriwal_sentiment,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), 
                                decreasing=TRUE))))
    
    ggplot(kejriwal_sentiment, aes(x=emotion))+
           geom_bar(aes(y=..count.., fill=emotion))+
        xlab("Classified by Emotions")+ylab("Number of Tweets")+ 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- KEJRIWAL")
    
    ggplot(kejriwal_sentiment, aes(x=polarity))+
           geom_bar(aes(y=..count.., fill=polarity))+ 
        xlab("Classified by Polarity")+ylab("Number of Tweets")+ 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- KEJRIWAL")


##--- classified all tweets related to Narendra Modi
##--- [Party:Bharatiya Janata Party(BJP)] 

    modi_emo = classify_emotion(modi_content, algorithm="voter", 
                                prior=1.0, verbose=FALSE)     
               emotion = modi_emo[,7]                                         
               emotion[is.na(emotion)] = "confuse"                             
    
    modi_pol = classify_polarity(modi_content, algorithm="voter")  
               polarity = modi_pol[,4]
    
    modi_sentiment = data.frame(text=modi_content, emotion=emotion,
                                polarity=polarity, stringsAsFactors=FALSE)
    modi_sentiment = within(modi_sentiment,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), 
                                   decreasing=TRUE))))
    
    ggplot(modi_sentiment, aes(x=emotion))+
           geom_bar(aes(y=..count.., fill=emotion))+
        xlab("Classified by Emotions")+ylab("Number of Tweets")+ 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- MODI")
    
    ggplot(modi_sentiment, aes(x=polarity))+
           geom_bar(aes(y=..count.., fill=polarity)) + 
        xlab("Classified by Polarity") + ylab("Number of Tweets") + 
    ggtitle("SENTIMENT ANALYSIS ON TWEETS- MODI")

#####################################################################
#                                                                   #
# STEP 4: ESTIMATE TWEET'S SENTIMENT                                #
#         PERFORMING "SENTIMENT SCORING ALGORITHM"(Lexicon Based)   #
#         (Following Breen's Approach)                              #
#                                                                   #  
#####################################################################

library(plyr)             # split up into small then combine back together again
library(stringr)          # deals with string in dataset       
library(graphics)

#--- Create function to rank the words from text, 
#--- based on dictionary of negative and postive words

    score.sentiment <- function(sentences,pos.words,neg.words,.progress='none')
      
        {
          
          scores <- laply(sentences, function(sentence, pos.words, neg.words)
            
              {
                sentence <- gsub('[[:punct:]]', "", sentence)
                sentence <- gsub('[[:cntrl:]]', "", sentence)
                sentence <- gsub('\\d+', "", sentence)
                sentence <- tolower(sentence)
              word.list <- str_split(sentence, '\\s+')
              words <- unlist(word.list)
                pos.matches <- match(words, pos.words)
                neg.matches <- match(words, neg.words)
                pos.matches <- !is.na(pos.matches)
                neg.matches <- !is.na(neg.matches)
              score <- sum(pos.matches) - sum(neg.matches)
            return(score)
                
              }, pos.words, neg.words, .progress=.progress)
          
              scores.df <- data.frame(score=scores, text=sentences)
          return(scores.df)
        }
    
    pos <- scan('C:/Users/NAFIS-NEHAN/Documents/positive-words.txt', 
                what='character', comment.char=';')
    neg <- scan('C:/Users/NAFIS-NEHAN/Documents/negative-words.txt', 
                what='character', comment.char=';') 
    
    pos.words = c(pos, 'upgrade')
    neg.words = c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

##--- Analysis the score to find strength of the Polarity

    rahul_analysis = score.sentiment(rahul_content,pos,neg,.progress='text')
        table(rahul_analysis$score)
        hist(rahul_analysis$score, col="blue1", border="grey", xlim= c(-4,4),lwd=2,
             main="STRENGTH of Polarity")
    
    kejriwal_analysis = score.sentiment(kejriwal_content,pos,neg,.progress='text')
        table(kejriwal_analysis$score)
        hist(kejriwal_analysis$score, col="dark green", border="grey", xlim= c(-4,4),lwd=2,
             main="STRENGTH of Polarity")
    
    modi_analysis = score.sentiment(modi_content,pos,neg,.progress='text')
        table(modi_analysis$score)
        hist(modi_analysis$score,  col="orange1", border="grey", xlim= c(-4,4),lwd=2,
             main="STRENGTH of Polarity")


####################################################################
#                                                                  #
# STEP 5: TEXT MINING-                                             #
#         PERFORM COMPARISON CLOUD TO VISUALIZE THE WORDS          #
#         & FIND MOST FREQUENT OCCURED WORDS IN TEXT               #
#                                                                  #
####################################################################

# library(??tm)             # already loaded with sentiment package; use for text analytics
library(SnowballC)          # stem words from string
library(slam)               # form row and col. sums
library(wordcloud)          # visualize most frequently occur words

      # 5(a)-- Rahul: Remove general english stop-word;
      #        list is tailored by adding unrelated words:

          rahul_content <- removeWords(rahul_content,c(stopwords("english"),"title", 
                      "dataexpandedurl","jsnav","dirltr","rt","rofl","href","the",
                    "dataquerysourcehashtagclick","targetblank","rpn","uknbspn",
                  "prenurserymommyoppn","thakrey","quotonerankonepensionquot",
                "classtwitteratreply","classtwitterhashtag","relnofollow",
              "classinvisiblenbspspanspana","classinvisiblespanspan","gandhi",
            "classtcoellipsisspan","classtwittertimelinelink","amp", "rahul",
              "classinvisible","classtcoellipsisspanspan","prettylink","barack",
                "hrefsearchqrgforsoldiersampsrchash","hrefsearchqrahulgandhiampsrchash",
                  "ssbincba","ssbrahulkanwalba","ssbrgeduba","will","obama",
                    "ssbyocongbjpsoconstitutionalba","ssbyouthempowermentbyba",                                                                                                                                    
                      "sbrahulroushanba","ssbrajivpartapba","ssbrgforsoldiersba",                                                                                                                                   
                    "ssbrginkarnatakaba","ssbrgroadshowinkarnatakaba","ssbrinadaveba",                                                                                                                                   
                  "ssbriteshjadavba","ssbrodhakumarba","ssbpandeypoonamnbtba",                                                                                                                          
                "ssbpardippandyaba","ssbparmarggarba","ssbpawankheraba",                                                                                                                                   
              "ssbpierrefitterba","ssbpandeypoonamnbtba","ssbsochiba",                                                                                                                       
            "ssbcongressmuktbharatba","ssbcoolfunnytshirtba"))

      # 5(b)-- Create corpus to keep text as docs.
  
      rahul_corpus = Corpus(VectorSource(rahul_content))  


#dataframe<-data.frame(text=unlist(sapply(mycorpus, `[`, "content")), 
 #                     stringsAsFactors=F)      
  
        dictcorpus <- rahul_corpus
          rahul_corpus <- tm_map(rahul_corpus, stemDocument,"english")    
            inspect(rahul_corpus[1:3])

      # 5(c)-- Create Term Document Matrix and display frequency of words in doc.
        rahul_tdm <- TermDocumentMatrix(rahul_corpus,
                       control = list(removePunctuation = TRUE,
                             removestripWhitespace = TRUE,
                             removeNumbers = TRUE, tolower = TRUE))
        print(rahul_tdm)
        
        rahul_m = as.matrix(rahul_tdm)
            word_freqs = sort(row_sums(rahul_m), decreasing=TRUE) 
                rahul_dm = data.frame(word=names(word_freqs), freq=word_freqs)

        wordcloud(rahul_dm$word, rahul_dm$freq, random.order=FALSE, 
            colors=brewer.pal(6, "Dark2"), min.freq=15, 
                scale=c(4,.2), rot.per=.20)

        max(apply(rahul_tdm,1,sum))  
            which(apply(rahul_tdm,1,sum)==2509)
                which(apply(rahul_tdm,1,sum)>300)

        findFreqTerms(rahul_tdm, lowfreq=150)
            findAssocs(rahul_tdm, 'gandhi', 0.15)
                sort(as.matrix(rahul_tdm)[1,], decreasing=T)[1:10]
                  inspect(rahul_tdm[100:104, 30:34])

      # 5(d)-- Kejriwal: Remove general english stop-word;
      #        list is tailored by adding unrelated words:
      #        follow the same steps as in Rahul

        kejriwal_content = removeWords(kejriwal_content,
                            c(stopwords("english"),"title","href","the",
                                 "datexpandedurl","prettylink","dirltr","sttions",
                                "relnofollow","dataexpandedurl","jsnav","targetblank",
                              "targetblank","ssbshameoncongbjpba","ssbratigirlba",
                            "hrefsearchqrgforsoldiersampsrchash","dataquerysourcehashtagclick",
                         "hrefsearchqrahulgandhiampsrchash","ssbyocongbjpsoconstitutionalba",                                                                                                                                             
                       "ssbyocongbjpsoconstitutionalbaa","ssbsavesandeshbadays",
                        "ssbsayba","ssbseemsokba","ssbshalupcrfba","ssbshameonbjpcongressba",                                                                                                                                                  
                          "ssbravimahoba","ssbravinderastroba","ssbravishkumarndtvba",                                                                                                                                                     
                            "ssbrealashokba","ssbreportervikrantba","ssblatewalaexcuseba",                                                                                                                                                      
                              "ssblessonba","ssbliarba","ssblkrajgseba","ssbmadhukishwarba",                                                                                                                                                        
                                "ssbmaheshjaggaba","ssbmanishkbaidba","ssbmanjultoonsba",                                                                                                                                                        
                                  "ssbmayankgandhiba","ssbmechirubhatba","samjha","savecr",
                                "classtwitteratreply","classtwitterhashtag","will",
                              "classinvisiblenbspspanspana","classinvisiblespanspan",
                            "classtcoellipsisspan","classtwittertimelinelink", 
                          "classinvisible","datexpandedurl","dataexpandedurl",                        "classtcoellipsisspanspan","hrefsearchqrgforsoldiersampsrchash",
                      "dataquerysourcehashtagclick","hrefarvindkejriw","amp",
                    "ssbarvindkejriwalba","aap","ssbaamaadmipartyba","ssbaapba",
                  "hrefaamaadmiparti","hrefsearchqaapampsrchash"))

        kejriwal_corpus = Corpus(VectorSource(kejriwal_content))

        dictcorpus <- kejriwal_corpus
          kejriwal_corpus <- tm_map(kejriwal_corpus, stemDocument, "english")    
            inspect(kejriwal_corpus[1:3])

        kejriwal_tdm = TermDocumentMatrix(kejriwal_corpus,
                         control = list(removePunctuation = TRUE,
                               removestripWhitespace = TRUE,
                               removeNumbers = TRUE, tolower = TRUE))

        print(kejriwal_tdm)

        kejriwal_m = as.matrix(kejriwal_tdm)
            word_freqs = sort(rowSums(kejriwal_m), decreasing=TRUE) 
                kejriwal_dm = data.frame(word=names(word_freqs), freq=word_freqs)


        wordcloud(kejriwal_dm$word, kejriwal_dm$freq, random.order=FALSE, 
            colors=brewer.pal(6, "Dark2"), min.freq=30, 
                scale=c(6,.4),rot.per=.15)

        max(apply(kejriwal_tdm,1,sum))  
            which(apply(kejriwal_tdm,1,sum)==2396)
                which(apply(kejriwal_tdm,1,sum)>500)

        findFreqTerms(kejriwal_tdm, lowfreq=30)
            findAssocs(kejriwal_tdm, 'hrefarvindkejriw', 0.12)
              sort(as.matrix(kejriwal_tdm)[1,], decreasing=T)[1:7]
                inspect(kejriwal_tdm[10:14, 21:30])

      # 5(d)-- modi: Remove general english stop-word;
      #        list is tailored by adding unrelated words:
      #        follow the same steps as in Rahul

        modi_content = removeWords(modi_content,
                        c(stopwords("english"), "for","title", "href","the","dirltr",
                             "classtwitteratreply", "classtwitterhashtag",
                            "classinvisiblenbspspanspana","classinvisiblespanspan",
                           "classtcoellipsisspan", "classtwittertimelinelink", 
                          "classinvisible","datexpandedurl", "prettylink",
                            "relnofollow","dataexpandedurl","jsnav",
                              "targetblank","classtcoellipsisspanspan","narendra",
                                "hrefsearchqrgforsoldiersampsrchash","dataquerysourcehashtagclick",
                              "hrefsearchqrahulgandhiampsrchash","ssbarvindatciiba",
                            "ssbaamaadmipartyba","ssbaapba","ssbaapyogendraba",                      
                          "ssbarvindkejriwalba","ssbashutoshbba","ssbbduttba",               
                        "ssbrahulkanwalba","ssbsardesairajdeepba","ssbshaziailmiba",
                      "ssbnarendramodiba","hrefnarendramodi","will","amp",
                      "datapreembeddedtru","mumbai","ralli","congress"))

        modi_corpus = Corpus(VectorSource(modi_content))

        dictcorpus <- modi_corpus
            modi_corpus <- tm_map(modi_corpus, stemDocument,"english")    
              inspect(modi_corpus[1:3])

        modi_tdm = TermDocumentMatrix(modi_corpus,
                    control = list(removePunctuation = TRUE,
                          removestripWhitespace = TRUE,
                          removeNumbers = TRUE, tolower = TRUE))

        modi_m = as.matrix(modi_tdm)
            word_freqs = sort(rowSums(modi_m), decreasing=TRUE) 
              modi_dm = data.frame(word=names(word_freqs), freq=word_freqs)

        wordcloud(modi_dm$word, modi_dm$freq, random.order=FALSE, 
              colors=brewer.pal(6, "Dark2"), min.freq=15, 
                  scale=c(4,.2),rot.per=.15)

        max(apply(modi_tdm,1,sum))  
            which(apply(modi_tdm,1,sum)==43)
                which(apply(modi_tdm,1,sum)>30)

        findFreqTerms(modi_tdm, lowfreq=30)
            findAssocs(modi_tdm, 'modi', 0.30)
                sort(as.matrix(modi_tdm)[1,], decreasing=T)[1:7]
                    inspect(modi_tdm[10:15, 50:55])


#####################################################################
#                                                                   #
# STEP 6: ANALYSIS FREQ. OF WORDS OF EACH CANDIDATE                 #
#                                                                   #
#####################################################################

        rahul.= paste(rahul_content, collapse=" ")
        kejriwal.= paste(kejriwal_content, collapse=" ")
        modi.= paste(modi_content, collapse=" ")
      
      # put everything in a single vector
        all = c(rahul., kejriwal., modi.)
      
      # remove stop-words
        all = removeWords(all,
                  c(stopwords("english"),"rahul","kejriwal","modi","also",
                          "for","title", "href","the","dirltr","amp","hrefndtv",
                          "classtwitteratreply","classtwitterhashtag","ssbntvba",
                          "classinvisiblenbspspanspana","classinvisiblespanspan",
                          "classtcoellipsisspan","classtwittertimelinelink", 
                          "classinvisible","datexpandedurl","prettylink","dataexpandedurl",
                          "relnofollow","jsnav","hrefsearchqindiaampsrchash",
                          "targetblank","classtcoellipsisspanspan","will","can",
                          "datapreembeddedtrue","ssbindiabacountry"))
      
        all_corpus = Corpus(VectorSource(all))
        all_tdm = TermDocumentMatrix(all_corpus)
        all_tdm = as.matrix(all_tdm)
      
        colnames(all_tdm) = c("rahul.", "kejriwal.", "modi.")
      
      # comparison cloud
        comparison.cloud(all_tdm, random.order=FALSE, 
                         colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                         title.size=1.5, rot.per=.15, max.words=300)
      
      # commonality cloud
        commonality.cloud(all_tdm, random.order=FALSE, 
                          colors = c(brewer.pal(12, "Dark2"),min.freq=200),
                          scale=c(4,.2),title.size=2.5,rot.per=.15)


#####################################################################
#                                                                   #
# STEP 7: NAIVE BAYES CLASSIFIER(cHECK PREDICTIONS AGAINST REALITY) #
#                                                                   #
#####################################################################

library("klaR")         # implementation of naive bayes in R
library("caret")        # experimental design
#library("graphics")     # S-like graphics helps to create graphics


      rahul_sentiment$polarity <- as.factor(rahul_sentiment$polarity)
        str(rahul_sentiment)

    # Divide, randomly select data, based on 80:20 ratio; 
      rn_train<-sample(nrow(rahul_sentiment), floor(nrow(rahul_sentiment)*0.8))
        train <- rahul_sentiment[rn_train,]
        test <- rahul_sentiment[-rn_train,]
      model<- NaiveBayes(polarity~.,data=train)

      prediction <- predict(model, test)
      confusionMatrix(prediction$class, prediction$class)

    # Follow the same steps
      kejriwal_sentiment$polarity <- as.factor(kejriwal_sentiment$polarity)
        str(kejriwal_sentiment)
      
      rn_train<-sample(nrow(kejriwal_sentiment), floor(nrow(kejriwal_sentiment)*0.8))
        train <- kejriwal_sentiment[rn_train,]
        test <- kejriwal_sentiment[-rn_train,]
      model<- NaiveBayes(polarity~.,data=train)
      
      prediction <- predict(model, test)
      confusionMatrix(prediction$class, prediction$class)
      
    # Follow the same steps
      modi_sentiment$polarity <- as.factor(modi_sentiment$polarity)
      
      str(modi_sentiment)
      
      rn_train <- sample(nrow(modi_sentiment), floor(nrow(modi_sentiment)*0.8))
        train <- modi_sentiment[rn_train,]
        test <- modi_sentiment[-rn_train,]
      model <- NaiveBayes(polarity~.,data=train)
      
      prediction <- predict(model, test)
      confusionMatrix(prediction$class, prediction$class)


#####################################################################
#####################################################################                                                                   #
##                                END                              ##                
#####################################################################                                                                   #
#####################################################################
