#The text is extracted from 
"http://en.wikipedia.org/wiki/Bird
http://en.wikipedia.org/wiki/Modern_birds
http://en.wikipedia.org/wiki/Twenty20
http://en.wikipedia.org/wiki/One_Day_International"
doc1<-"Each bowler may bowl a maximum of only one-fifth of the total overs per innings. For a full, uninterrupted match, this is 4 overs.
If a bowler delivers a no ball by overstepping the popping crease, it costs 1 run and his next delivery is designated a free-hit. 
In this circumstance the batsman can only be dismissed through a run out, hitting the ball twice, obstructing the field or handling the ball.
The following fielding restrictions apply:No more than five fielders can be on the leg side at any time.During the first six overs, a maximum of two fielders can be outside the 30-yard circle (this is known as the powerplay).
        After the first six overs, a maximum of five fielders can be outside the fielding circle."
doc2<-"An ODI is contested by 2 teams of 11 players each.The Captain of the side winning the toss chooses to either bat or bowl (field) first.
The team batting first sets the target score in a single innings. The innings lasts until the batting side is all out (i.e., 10 of the 11 batting players are out) or all of the first side's allotted overs are completed.
Each bowler is restricted to bowling a maximum of 10 overs (fewer in the case of rain-reduced matches and in any event generally no more than one fifth or 20% of the total overs per innings). Therefore, each team must comprise at least five competent bowlers (either dedicated bowlers or all-rounders).
The team batting second tries to score more than the target score in order to win the match. Similarly, the side bowling second tries to bowl out the second team or make them exhaust their overs before they reach the target score in order to win.
If the number of runs scored by both teams is equal when the second team loses all of its wickets or exhausts all its overs, then the game is declared a tie (regardless of the number of wickets lost by either team).
"
doc3<-"Modern birds are characterised by a beak with no teeth and a high 
metabolic rate and rate of growth. Most can fly, with some exceptions 
including ratites, penguins, and a number of diverse endemic island species. 
Modern birds also have unique digestive and respiratory systems that are highly 
adapted for flight. Some birds, especially corvids and parrots, are among the most 
intelligent animal species; a number of bird species have been observed manufacturing and using tools, and many social
species exhibit cultural transmission of knowledge across generations."
doc4<-"Birds (class Aves or clade Avialae) are feathered, winged, two-legged, 
warm-blooded, egg-laying vertebrates. Aves ranks as the tetrapod class with 
the most living species, approximately ten thousand. Extant birds belong to the 
subclass Neornithes, living worldwide and ranging in size from the 5 cm (2 in) bee 
hummingbird to the 2.75 m (9 ft) ostrich. The fossil record indicates that birds emerged 
within the theropod dinosaurs during the Jurassic period, around 150 million years ago. 
Most researchers agree that modern-day birds are the only living members of the 
Dinosauria clade."

doc.list <- list(doc1, doc2,doc3,doc4)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))
library(tm)
my.corpus <- Corpus(my.docs)
my.corpus <- tm_map(my.corpus, removePunctuation)
library(SnowballC)

my.corpus <- tm_map(my.corpus, removeNumbers)#remove numbers
my.corpus <- tm_map(my.corpus, tolower)#to lowercase
my.corpus <- tm_map(my.corpus, stripWhitespace)#remove extra white space

#remove stop words from all documents
my.corpus$doc1<-removeWords(my.corpus$doc1, stopwords("english"))
my.corpus$doc2<-removeWords(my.corpus$doc2, stopwords("english"))
my.corpus$doc3<-removeWords(my.corpus$doc3, stopwords("english"))
my.corpus$doc4<-removeWords(my.corpus$doc4, stopwords("english"))

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
#inspect(term.doc.matrix.stm)
kmeans5<- kmeans(term.doc.matrix.stm, 2)
plotcluster(term.doc.matrix.stm, kmeans5$cluster)
inspect(term.doc.matrix.stm[which((kmeans5$cluster==1)),])
#find freq of a term across all documents
new<-rowSums(as.matrix(term.doc.matrix.stm))
#term.doc.matrix.stm<-cbind(term.doc.matrix.stm,new)
#find term with atleast freq =4 and belong to cluster 1
inspect(term.doc.matrix.stm[which((kmeans5$cluster==1)&(new)>3),])
plot(kmeans5)
#find term with atleast freq =4 and belong to cluster 2
inspect(term.doc.matrix.stm[which((kmeans5$cluster==2)&(new)>3),])

