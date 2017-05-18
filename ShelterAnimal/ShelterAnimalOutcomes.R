library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(dplyr)
library(RGtk2)
library(caret)

#set working directory
setwd("/Users/yifan/Documents/Data_Analytics/DAFinal_Project")

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
summary(train)
summary(test)
str(train)
str(test)
table(train$OutcomeSubtype)
table(train$OutcomeType)
table(train$AnimalType)
table(train$SexuponOutcome)
table(train$AgeuponOutcome)
table(train$Breed)
table(train$Color)

#test NA or "" data
count <- 0
for(i in combine$AgeDay){
  if(is.na(i)){
    count <- count+1
  } 
}
count
count <- 0
for(i in combine$OutcomeType){
  if(i==''){
    count <- count+1
  } 
}
count

#data cleaning
train1 <- train
test1 <- test
train1$OutcomeSubtype <- NULL
test1$OutcomeType <- NA
colnames(train1)[1] <- "ID"
combine <- rbind(train1, test1)

combine$HasName[combine$Name == ""] <- 0
combine$HasName[!combine$Name == ""] <- 1

#split SexuponOutcome into Intact and Gender
combine$SexuponOutcome[combine$SexuponOutcome==''] <- 'Unknown'
combine$SexIntact[grepl("Intact", combine$SexuponOutcome)] <- 'Intact'
combine$SexIntact[grepl("Neutered", combine$SexuponOutcome)] <- 'NotIntact'
combine$SexIntact[grepl("Spayed", combine$SexuponOutcome)] <- 'NotIntact'
combine$SexIntact[grepl("Unknown", combine$SexuponOutcome)] <- 'Unknown'
combine$SexGender[grepl("Female", combine$SexuponOutcome)] <- 'Female'
combine$SexGender[grepl("Male", combine$SexuponOutcome)] <- 'Male'
combine$SexGender[grepl("Unknown", combine$SexuponOutcome)] <- 'Unknown'
table(combine$OutcomeType)
table(combine$AnimalType)
table(combine$SexuponOutcome)
table(combine$SexIntact)
table(combine$SexGender)

#split Color into Type and Pattern
combine$ColorType <- sapply(combine$Color,function(x) strsplit(x, split = '/| ')[[1]][1])
combine$ColorSplit <- sapply(strsplit(combine$Color," "),'[',2)
combine$ColorSplit[is.na(combine$ColorSplit)] <- "Unknown"
combine$ColorPattern <- sapply(combine$ColorSplit,function(x) strsplit(x, split = '/')[[1]][1])
combine$ColorMulti[!grepl("/", combine$Color)] <- 0
combine$ColorMulti[grepl("/", combine$Color)] <- 1
table(combine$ColorType)
table(combine$ColorPattern)

#classify Breed variable
combine$BreedMix[!grepl("Mix", combine$Breed)] <- 0
combine$BreedMix[grepl("Mix", combine$Breed)] <- 1
combine$BreedSplit <- sapply(combine$Breed,function(x) strsplit(x, split = '/')[[1]][1])
table(combine$BreedSplit[combine$AnimalType == 'Cat'])
combine$BreedGroup <- NA
combine$BreedGroup[grepl("Abyssinian", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("American Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("American Wirehair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'MediumHair'
combine$BreedGroup[grepl("Angora", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Balinese", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Bengal", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'MediumHair'
combine$BreedGroup[grepl("Bombay", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("British Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Burmese", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Chartreux", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Cornish Rex", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Cymric", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Devon Rex", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Domestic Longhair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Domestic Medium Hair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'MediumHair'
combine$BreedGroup[grepl("Domestic Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Exotic Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Havana Brown", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Himalayan", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Japanese Bobtail", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Javanese", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Maine Coon", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Manx", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Munchkin Longhair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Munchkin Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Norwegian Forest", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Ocicat", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Oriental Sh", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Persian", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Pixiebob Shorthair", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Ragdoll", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Russian Blue", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Scottish Fold", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Siamese", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Snowshoe", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'ShortHair'
combine$BreedGroup[grepl("Sphynx", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'Hairless'
combine$BreedGroup[grepl("Tonkinese", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'MediumHair'
combine$BreedGroup[grepl("Turkish Angora", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
combine$BreedGroup[grepl("Turkish Van", combine$BreedSplit) & combine$AnimalType == 'Cat'] <- 'LongHair'
table(combine$BreedGroup)
table(combine$AnimalType)

table(combine$BreedSplit[combine$AnimalType == 'Dog'])
combine$BreedGroup[grepl("Affenpinscher", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Afghan Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Airedale Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Akita", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Alaskan Husky", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Alaskan Malamute", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("American Bulldog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("American Eskimo", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("American Foxhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("American Pit Bull Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("American Staffordshire Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Anatol Shepherd", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Australian Cattle Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Australian Kelpie", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Australian Shepherd", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Australian Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Basenji", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Basset Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Beagle", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Bearded Collie", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Beauceron", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Bedlington Terr", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Belgian Malinois", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Belgian Sheepdog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Belgian Tervuren", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Bernese Mountain Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Bichon Frise", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Black", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Black Mouth Cur", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Bloodhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Blue Lacy", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Bluetick Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Boerboel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Border Collie", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Border Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Boston Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Boxer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Boykin Span", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Brittany", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Bruss Griffon", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Bull Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Bulldog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Bullmastiff", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Cairn Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Canaan Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Cane Corso", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Cardigan Welsh Corgi", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Carolina Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Catahoula", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Cavalier Span", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Chesa Bay Retr", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Chihuahua Longhair", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Chihuahua Shorthair", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Chinese Crested", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Chinese Sharpei", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Chow Chow", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Cocker Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Collie Rough", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Collie Smooth", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Coton De Tulear", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Dachshund", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Dalmatian", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Dandie Dinmont", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Doberman Pinsch", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Dogo Argentino", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Dogue De Bordeaux", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Dutch Shepherd", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Eng Toy Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("English Bulldog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("English Cocker Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("English Coonhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("English Foxhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("English Pointer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("English Setter", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("English Shepherd", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("English Springer Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Entlebucher", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Feist", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Field Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Finnish Spitz", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Flat Coat Retriever", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("French Bulldog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("German Pinscher", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("German Shepherd", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("German Shorthair Pointer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("German Wirehaired Pointer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Glen Of Imaal", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Golden Retriever", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Gordon Setter", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Great Dane", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Great Pyrenees", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Greater Swiss Mountain Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Greyhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Harrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Havanese", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Hovawart", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Ibizan Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Irish Setter", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Irish Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Irish Wolfhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Italian Greyhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Jack Russell Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Japanese Chin", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Jindo", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Keeshond", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Kuvasz", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Labrador Retriever", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Landseer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Leonberger", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Lhasa Apso", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Lowchen", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Maltese", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Manchester Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Mastiff", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Mexican Hairless", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Miniature Pinscher", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Miniature Poodle", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Miniature Schnauzer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Neapolitan Mastiff", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Newfoundland", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Norfolk Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Norwegian Elkhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Norwich Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Nova Scotia Duck Tolling Retriever", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Old English Bulldog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Old English Sheepdog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Otterhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Papillon", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Parson Russell Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Patterdale Terr", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Pbgv", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Pekingese", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Pembroke Welsh Corgi", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Pharaoh Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Picardy Sheepdog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Pit Bull", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Plott Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Podengo Pequeno", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Pointer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Pomeranian", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Port Water Dog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Presa Canario", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Pug", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Queensland Heeler", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Rat Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Redbone Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Rhod Ridgeback", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Rottweiler", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Saluki", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Samoyed", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Schipperke", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Schnauzer Giant", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Scottish Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Sealyham Terr", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Shetland Sheepdog", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Shiba Inu", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Shih Tzu", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Siberian Husky", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Silky Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Skye Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Smooth Fox Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Soft Coated Wheaten Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Spanish Mastiff", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Spinone Italiano", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("St. Bernard Rough Coat", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("St. Bernard Smooth Coat", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Staffordshire", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Standard Poodle", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Standard Schnauzer", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Working'
combine$BreedGroup[grepl("Swedish Vallhund", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Herding'
combine$BreedGroup[grepl("Swiss Hound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Tibetan Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Tibetan Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'NonSporting'
combine$BreedGroup[grepl("Toy Fox Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Toy Poodle", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
combine$BreedGroup[grepl("Treeing Cur", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Treeing Tennesse Brindle", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Treeing Walker Coonhound", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Unknown", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Unknown'
combine$BreedGroup[grepl("Vizsla", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Weimaraner", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Welsh Springer Spaniel", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Welsh Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("West Highland", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Whippet", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Hound'
combine$BreedGroup[grepl("Wire Hair Fox Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Terrier'
combine$BreedGroup[grepl("Wirehaired Pointing Griffon", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Sporting'
combine$BreedGroup[grepl("Yorkshire Terrier", combine$BreedSplit) & combine$AnimalType == 'Dog'] <- 'Toy'
table(combine$BreedGroup)
table(combine$AnimalType)
str(combine)

#split DateTime into hour, month, year
combine$DateTimeSplit1 <- sapply(combine$DateTime,function(x) strsplit(x, split = ' ')[[1]][1])
combine$DateTimeSplit2 <- sapply(combine$DateTime,function(x) strsplit(x, split = ' ')[[1]][2])
combine$DateTimeHour <- sapply(combine$DateTimeSplit2,function(x) strsplit(x, split = ':')[[1]][1])
combine$DateTimeMonth <- sapply(combine$DateTimeSplit1,function(x) strsplit(x, split = '-')[[1]][2])
combine$DateTimeYear <- sapply(combine$DateTimeSplit1,function(x) strsplit(x, split = '-')[[1]][1])

#unify the unit
combine$Value <- sapply(combine$AgeuponOutcome, function(x) strsplit(x, " ")[[1]][1])
combine$Value <- as.numeric(combine$Value)
combine$Unit <- sapply(combine$AgeuponOutcome, function(x) strsplit(x, " ")[[1]][2])
combine$Unit <- gsub("s","",combine$Unit)
combine$Unit <- as.factor(combine$Unit)
combine$Day <- ifelse(combine$Unit == 'day', 1,
                      ifelse(combine$Unit == 'week', 7,
                             ifelse(combine$Unit == 'month', 30,
                                    ifelse(combine$Unit == 'year', 365, NA))))
combine$AgeDay <- combine$Value*combine$Day

#fill the empty value in AgeDay
treeModel_AgeDay <- rpart(AgeDay ~ AnimalType+SexIntact+SexGender+ColorType+ColorPattern+ColorMulti+BreedMix+BreedGroup+HasName+DateTimeHour+DateTimeMonth,
                          data=combine[!is.na(combine$AgeDay),],
                          method="anova")
combine$AgeDay[is.na(combine$AgeDay)] <- predict(treeModel_AgeDay, combine[is.na(combine$AgeDay),])

#create a new attribute to calssify animals' age
combine$AgeGroup[combine$AgeDay <= 365] <- 'baby'
combine$AgeGroup[combine$AgeDay > 365] <- 'adult'
combine$AgeGroup <- factor(combine$AgeGroup)

#create a new attribute TimeofDay indicating when the animal was sent in
combine$TimeofDay <- ifelse(combine$DateTimeHour > 5 & combine$DateTimeHour <= 10, 'morning',
                         ifelse(combine$DateTimeHour > 10 & combine$DateTimeHour <= 15, 'midday',
                                ifelse(combine$DateTimeHour > 15 & combine$DateTimeHour <= 20, 'lateday', 'night')))
combine$TimeofDay <- factor(combine$TimeofDay, 
                         levels = c('morning', 'midday',
                                    'lateday', 'night'))

#create a Season attribute
combine$Season <- ifelse(combine$DateTimeMonth >= 3 & combine$DateTimeMonth <= 5, 'spring',
                            ifelse(combine$DateTimeMonth >= 6 & combine$DateTimeHour <= 8, 'summer',
                                   ifelse(combine$DateTimeMonth >= 9 & combine$DateTimeHour <= 11, 'fall', 'winter')))
combine$Season <- factor(combine$Season)

#change data as factor
write.csv(combine,file="combine.csv",row.names = FALSE)
combine <- read.csv("combine.csv", stringsAsFactors=TRUE)

#split combine dataset into train and test
train_p1 <- combine[!is.na(combine$OutcomeType),]
test_p1 <- combine[is.na(combine$OutcomeType),]

#DecisionTree Model
treeModel <- rpart(OutcomeType ~ AnimalType+AgeDay+HasName+SexIntact+SexGender+ColorType+ColorPattern+ColorMulti+BreedGroup+BreedMix+DateTimeHour+DateTimeMonth+DateTimeYear,
                   data = train_p1,
                   method="class")
#confusionMatrix
Prediction1_train <- predict(treeModel, train_p1, type="class")
confusionMatrix(Prediction1_train, train_p1$OutcomeType)
#Prediction
Prediction1 <- predict(treeModel, test_p1, type="prob")
submit <- data.frame(ID = test_p1$ID, Prediction1)
str(submit)
write.csv(submit,file="rpartDecisionTree1.csv",row.names = FALSE)
#Plot
fancyRpartPlot(treeModel)

#RandomForest Model1
my_forest1 <- randomForest(OutcomeType ~ AnimalType+AgeDay+HasName+SexIntact+SexGender+ColorType+ColorPattern+ColorMulti+BreedGroup+BreedMix+DateTimeHour+DateTimeMonth+DateTimeYear,
                           data = train_p1,
                           importance = TRUE,
                           ntree = 1000)
#confusionMatrix
my_prediction1_train <- predict(my_forest1, train_p1, type="class")
confusionMatrix(my_prediction1_train, train_p1$OutcomeType)
#Prediction
my_prediction1 <- predict(my_forest1, test_p1, type="prob")
submit1 <- data.frame(ID = test_p1$ID, my_prediction1)
write.csv(submit1,file="randomForest_forest1.csv",row.names = FALSE)
#Importance Analysis
importance <- importance(my_forest1)
varImportance <- data.frame(Variables = row.names(importance),Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- mutate(varImportance,Rank = paste0('#',dense_rank(desc(Importance))))
Var <- c(rankImportance$Rank)
ggplot(rankImportance, aes(x = reorder(Variables, Importance),y = Importance,fill = Var)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender',
            fontface = 'bold') +
  labs(x = 'Variables') +
  coord_flip()

#RandomForest Model2
my_forest2 <- randomForest(OutcomeType ~ AnimalType+AgeGroup+HasName+SexIntact+SexGender+ColorType+ColorPattern+ColorMulti+BreedGroup+BreedMix+TimeofDay+Season+DateTimeYear,
                                          data = train_p1,
                                          importance = TRUE,
                                          ntree = 1000)
#confusionMatrix
my_prediction2_train <- predict(my_forest2, train_p1, type="class")
confusionMatrix(my_prediction2_train, train_p1$OutcomeType)
#Prediction
my_prediction2 <- predict(my_forest2, test_p1, type="prob")
submit2 <- data.frame(ID = test_p1$ID, my_prediction2)
write.csv(submit2,file="randomForest_forest2.csv",row.names = FALSE)