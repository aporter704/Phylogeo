library(ape)

tFiles <- dir(pattern='.+txt')
codesVic <- lapply(tFiles, function(x) read.delim(x, head=F))
names(codesVic) <- tFiles

aln <- read.dna('vic_travelhist.fasta', format='fasta')
# starting format e.g.: "hCoV-19/Australia/VIC754/2020|EPI_ISL_427031|2020-03-30"
# desired format: EPI#_VIC#_travelhistorylocation_date

# converting vicCodes to a vector with names set as travel history
codesVic <- unlist(codesVic)
names(codesVic) <- gsub(names(codesVic), pattern='.txt.V.+', replacement='')

# getting rest of data (Date & GISAID accession from alignment headers)
namesDataAln <- strsplit(names(aln), '\\|')

# vector to store new headers 
newName <- vector()
# vector for no match samples
noMatch <- vector()
match <- vector()

for (i in 1:length(codesVic)){

	if (!any(grepl(namesDataAln, pattern=paste0('/', codesVic[i], '/')))) {

		noMatch <- c(noMatch, i)

	} else {

		metaData <- namesDataAln[[which(grepl(namesDataAln, pattern=paste0('/', codesVic[i], '/')))]]
		print(which(grepl(namesDataAln, pattern=paste0('/', codesVic[i], '/'))))
		#print(paste0('MATCH: ', metaData))
		epi <- metaData[which(grepl(metaData, pattern='EPI'))]
		vic <- codesVic[i]
		origin <- names(codesVic)[i]
		date <- metaData[length(metaData)]
		match <- c(match, i)
		newName <- c(newName, paste0(epi, '_', vic, '_', origin, '_', date))

		names(aln)[i] <- paste0(epi, '_', vic, '_', origin, '_', date)

	}

}

saveAln <- aln[names(aln)[match]]
write.dna(saveAln, 'travelHistVisSamples.fasta', format='fasta')


