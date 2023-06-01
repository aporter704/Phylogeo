library(ggplot2)
library(tidyverse)
library(viridis)
library(lubridate)
library(beastio)

jFiles <- dir(pattern = ".+jumpTimes.txt", path = "./log", full.names = TRUE)

data <- lapply(jFiles, function(x) as_tibble(read.delim(x, header = TRUE)))
names(data) <- gsub(
	dir(pattern = ".+jumpTimes.txt", path = "./log"),
	pattern = "jumpTimes[.]txt",
	replacement = ""
)

data <- bind_rows(data, .id = "id")

## find MAP states
logFiles <- dir(
	pattern = "DTA[.]log|_rep.[.]log",
	path = "./log",
	full.names = TRUE,
)

logs <- lapply(logFiles, function(x) as_tibble(beastio::readLog(x, as.mcmc = FALSE, burnin = 0.1)))
names(logs) <- gsub(
	dir(pattern = "DTA[.]log|_rep.[.]log", path = "./log"),
	pattern = "[.]log",
	replacement = ""
)

logs <- bind_rows(logs, .id = "id")

mapStates <- logs %>%
	group_by(id) %>%
	filter(joint == max(joint)) %>%
	select(id, state) %>%
	mutate(combo = paste0(id, state))

# pull map state - very slow computation!
toAus <- data %>%
	filter(to == "Australia") %>%
	mutate(combo = paste0(id, state)) %>%
	filter(combo %in% mapStates$combo) %>%
	mutate(jump = paste0(from, " -> ", to)) %>%
	mutate(date = as.Date(date_decimal((decimal_date(dmy("31-05-2020")) - time))))

# prelim plots
pdf("allJumps.pdf", useDingbats = FALSE, width = 12, height = 4)
	ggplot(toAus, aes(x = date, fill = jump)) +
		geom_area(stat = "bin", binwidth = 14) +
		scale_fill_manual(values = viridis_pal(option = "plasma")(8)) +
		facet_wrap(~id, scales = "free_y") +
		scale_x_date(labels = scales::date_format("%b %Y"))
dev.off()

# joint probability
pdf("joint.pdf", useDingbats = FALSE)
	ggplot(logs) +
		geom_histogram(aes(x = joint), binwidth = 1) +
		facet_wrap(~ id)
dev.off()











