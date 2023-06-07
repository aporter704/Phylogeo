library(ape)

tFiles <- dir(pattern='.+txt')
codesVic <- lapply(tFiles, function(x) read.delim(x, head=F))
names(codesVic) <- tFiles

aln <- read.dna('vic_travelhist.fasta', format='fasta')

#aln2 <- read.dna('travelHistVisSamples.fasta', format='fasta')
# starting format e.g.: "hCoV-19/Australia/VIC754/2020|EPI_ISL_427031|2020-03-30"
# desired format: EPI#_VIC#_travelhistorylocation_date

# converting vicCodes to a vector with names set as travel history
codesVic <- unlist(codesVic)
names(codesVic) <- gsub(names(codesVic), pattern='.txt.V.+', replacement='')

# getting rest of data (Date & GISAID accession from alignment headers)
namesDataAln <- strsplit(names(aln), '\\|')
namesDataAln <- lapply(namesDataAln, function(x) gsub(x, pattern='.+Australia/', replacement=''))
namesDataAln <- lapply(namesDataAln, function(x) gsub(x, pattern='/.+', replacement=''))

# convert list to data frame
alnHeaderDF <- data.frame()
for (i in 1:length(namesDataAln)){
	alnHeaderDF <- rbind(alnHeaderDF, namesDataAln[[i]])
}
colnames(alnHeaderDF) <- c('vicCode', 'gisaidAcc', 'date')
# removing the '_' in dates
alnHeaderDF$date <- gsub(alnHeaderDF$date, pattern='_', replacement='')

# changing alignment headers now
match <- vector()
noMatch <- vector()

for (i in 1:dim(alnHeaderDF)[1]){
	if (alnHeaderDF$vicCode[i] %in% codesVic) {
		sampNo <- which((codesVic %in% alnHeaderDF$vicCode[i]))
		# - OLD WRONG COMMAND match <- c(match, sampNo)
		match <- c(match, i)
		names(aln)[i] <- paste0(alnHeaderDF$gisaidAcc[i], '_', alnHeaderDF$vicCode[i], '_',
			names(codesVic)[sampNo], '_', alnHeaderDF$date[i])
		#print(paste0(alnHeaderDF$gisaidAcc[i], '_', alnHeaderDF$vicCode[i], '_',
		#	names(codesVic)[sampNo], '_', alnHeaderDF$date[i]))
	} else {
		noMatch <- c(noMatch, i)
	}
}

write.dna(aln[names(aln)[match]], 'vicTravelHistFormatted.fasta', format='fasta')
























