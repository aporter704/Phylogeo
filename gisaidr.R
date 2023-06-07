#install.packages("ape")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("devtools")

library(devtools)
#install_github("wytamma/GISAIDR")
library(GISAIDR)
library(lubridate)
library(ape)

exportFasta <- function(df, outfile = 'out.fasta'){
    cat('', file = outfile)
    notParsed <- 0
    for(i in 1:nrow(df)){
      date <- round(decimal_date(ymd(df_seq$date[i])), 2)
      if(is.na(date)){
          notParsed <- notParsed+1
          next
      }else{
          cat(paste0('>', df_seq$accession_id[i], '_', df_seq$pangolin_lineage[i],  '_',
                     gsub(' ', '', df_seq$country[i]),
                     '_', date, '\n'), append = T, file = outfile)
          cat(df_seq$sequence[i], '\n', append = T, file = outfile)
      }
    }
    cat(notParsed, ' not parsed')
}


username <- "aporter704"
password <- "FatCatBat55"

credentials <- login(username = username, password = password)

#query based on loaction
#Australia
#df <- query(credentials = credentials,  location = 'Australia/Victoria', from = '2020-01-01', to = '2020-06-01', nrows=5000)

#Asia
Asia <- c('Asia / Afghanistan',
          'Asia / Armenia',
          'Asia / Bahrain',
          'Asia / Bangladesh',
          'Asia / Brunei',
          'Asia / Cambodia',
          # 'Asia / China',
          'Asia / Georgia',
          'Asia / Hong Kong',
          'Asia / India',
          'Asia / Indonesia / ...',
          'Asia / Iran',
          'Asia / Iraq',
          'Asia / Israel',
          'Asia / Japan',
          'Asia / Jordan / ...',
          'Asia / Kazakhstan / ...',
          'Asia / Kuwait',
          'Asia / Lebanon',
          'Asia / Malaysia',
          'Asia / Mongolia',
          'Asia / Myanmar',
          'Asia / Nepal',
          'Asia / Oman / ...',
          'Asia / Pakistan',
          'Asia / Palestine / ...',
          'Asia / Philippines',
          'Asia / Qatar / ...',
          'Asia / Saudi Arabia',
          'Asia / Singapore',
          'Asia / South Korea',
          'Asia / Sri Lanka',
          'Asia / Taiwan',
          'Asia / Thailand',
          'Asia / Timor-Leste',
          'Asia / United Arab Emirates',
          'Asia / Uzbekistan',
          'Asia / Vietnam')

credentials <- login(username = username, password = password)
data <- data.frame()
for (country in Asia) {
  print(country)
  df <- query(credentials = credentials, location = country,from = '2020-01-01', to = '2020-06-01', nrows=10000)
  data <- rbind(data, df)
}

dim(df)
#Seb edit: select only those with full collection date info
df$decimalDate <- decimal_date(ymd(df$collection_date))
df <- df[!is.na(df$decimalDate), ]

#check query is correct
head(df)

# Seb edit: I suggest first subsettin those within the date range that you want:
#dfPreJune2020 <- df[decimal_date(ymd(df$collection_date)) < decimal_date(ymd('2020-06-01')), ]
#dim(dfPreJune2020)


#randomly sample to 200 sequences
df200 <- df[sample(1:dim(df)[1], 200),]

head(df200)

#limit to first wave dates
accs_to_download <- df200$accession_id

accs_to_download <- accs_to_download[!is.na(accs_to_download)]

head(accs_to_download)

#download seqs
credentials <- login(username = username, password = password)

#test <- query(credentials = credentials)

df_seq <- download(credentials = credentials, list_of_accession_ids = accs_to_download, get_sequence = T)

exportFasta(df_seq, outfile = 'Asia.fasta')

# Check that it actually loads OK
testInput <- read.dna('Asia.fasta', format = 'fasta')
testInput
