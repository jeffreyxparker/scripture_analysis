### Scripture Analysis ###

# Libraries
library(readr)
library(tm)
library(stringr)
library(arules)
library(arulesViz)

# Data source: http://www.gutenberg.org/cache/epub/10/pg10.txt

# Load and clean the data
mystring <- read_file("C:/Users/Jeff/Desktop/pg10.txt") # Reading the bible as one long string
mystring <- gsub(pattern="\\r\\n",replacement=" ", mystring) # Replacing paragraphs characters
mystring <- gsub(pattern="\\d\\d?:\\d\\d?",replacement="|", mystring) # Replacing chapter:verse with pipe ("|")
mystring <- toupper(mystring) # Upper casing all the characters
newtestament <- t(do.call(rbind,strsplit(mystring,'|',fixed=T))) # Transforming so each verse is row
newtestament <- gsub("[[:punct:]]", "", newtestament) # Removing punctuation
newtestament <- trimws(newtestament) # Trimming the white space
head(newtestament, 10) 

# If anyone knows of a way to import files with multiple delimiters, that would be awesome. I spent way too much time on that. I tried read_delim(), but couldn't get it to work.

# Calculate the most common words
corpus <- VCorpus(VectorSource(newtestament))
dtm <- DocumentTermMatrix(corpus)
dtmsort <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
dtmsort[1:50]
barplot(dtmsort[4:20], main = "Most Common Terms in New Testament", ylab = "Frequency of Occurance", xlab = "Terms")

# Calculate the most common 1st word
newtestament <- as.data.frame(newtestament)
newtestament$V2  <- gsub("([A-Za-z]+).*", "\\1", newtestament$V1)
mytable <- table(newtestament$V2)
tablesort <- sort(mytable, decreasing=TRUE)
barplot(tablesort[1:10], main = "Most Common First Terms in New Testament Verses", ylab = "Frequency of Occurance", xlab = "Terms")

# Calcuate the most common 2nd word
newtestament$V3  <- word(newtestament$V1, 2)
mytable <- table(newtestament$V3)
tablesort <- sort(mytable, decreasing=TRUE)
barplot(tablesort[1:10], main = "Most Common Second Terms in New Testament Verses", ylab = "Frequency of Occurance", xlab = "Terms")

# Calculate the words that most commonly follow "FOR"
newtestament$V5 <- str_extract(newtestament$V1, '(?<=FOR\\s)\\w+')
mytable <- table(newtestament$V5)
tablesort <- sort(mytable, decreasing=TRUE)
barplot(tablesort[1:10], main = "Most Common Terms following FOR", ylab = "Frequency of Occurance", xlab = "Terms")

# Cleanup
rm(mystring,mytable,newtestament,dtmsort,tablesort,corpus)

# Create a market basket
dtm1 <- as.matrix(dtm)
trans <- as(dtm1, "transactions") # The arules package uses a special data object called transactions instead of data frames
trans # My rows are verses and my columns are words
inspect(trans[1:5]) # Note that duplicate words have been removed
itemFrequencyPlot(trans, support = .1) # Plotting words that appear in 10% or more of the verses

sets <- apriori(trans, parameter = list(target = "freq")) # calculating the sets of words
inspect(sets)
inspect(head(sort(sets), n=10))
plot(sets[1:10], method = "graph") # Cool plot 


plot(rules, method="grouped", control=list(k=20)) # Cool plot

rules <- apriori(trans, parameter = list(
                                         target = "freq",
                                         conf = .5,
                                         support = .001,
                                         minlen=7,
                                         maxlen=15))
inspect(rules)
inspect(head(sort(rules), n=10))
plot(rules)
