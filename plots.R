library(ggplot2)
library(tidyverse)
library(viridis)
library(lubridate)
library(beastio)
library(svglite)

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

# pull map state
toAus <- data %>%
    filter(to == "Australia") %>%
    mutate(combo = paste0(id, state)) %>%
    filter(combo %in% mapStates$combo) %>%
    mutate(jump = paste0(from, " -> ", to)) %>%
    mutate(date = as.Date(date_decimal((decimal_date(dmy("31-05-2020")) - time))))

#save(data, logs, mapStates, file = "plotData.RData")
## preliminary plots plots
# load data if already exists
load("plotData.RData")
# else run the above to generate the required data objects: data, logs, mapStates

# all jumps
pdf("allJumps.pdf", useDingbats = FALSE, width = 12, height = 4)
ggplot(toAus, aes(x = date, fill = jump)) +
    geom_area(stat = "bin", binwidth = 7) +
    scale_fill_manual(values = viridis_pal(option = "plasma")(8)) +
    facet_wrap(~id, scales = "free_y") +
    scale_x_date(labels = scales::date_format("%b %Y"))
dev.off()

# joint probability
pdf("joint.pdf", useDingbats = FALSE)
ggplot(logs) +
    geom_histogram(aes(x = joint), binwidth = 1) +
    facet_wrap(~id)
dev.off()

# final trajectory plot
data %>%
    filter(to == "Australia") %>%
    mutate(combo = paste0(id, state)) %>%
    filter(combo %in% mapStates$combo) %>%
    mutate(jump = paste0(from, " -> ", to)) %>%
    mutate(date = as.Date(date_decimal((decimal_date(dmy("31-05-2020")) - time)))) %>%
    filter(id == "location.Feb_VIC_travelHistory_rep1") %>%
    ggplot(aes(x = date, fill = jump)) +
    geom_area(stat = "bin", binwidth = 7) +
    scale_fill_manual(values = viridis_pal(option = "plasma")(8), name = "") +
    scale_x_date(labels = scales::date_format("%b %Y")) +
    ylab("Weekly jumps") +
    geom_vline(xintercept = as.Date(date_decimal(decimal_date(dmy("1-02-2020")))), color = "purple", linetype = "dashed") +
    geom_vline(xintercept = as.Date(date_decimal(decimal_date(dmy("29-02-2020")))), color = "darkmagenta", linetype = "dashed") +
    geom_vline(xintercept = as.Date(date_decimal(decimal_date(dmy("05-03-2020")))), color = "darkblue", linetype = "dashed") +
    geom_vline(xintercept = as.Date(date_decimal(decimal_date(dmy("11-03-2020")))), color = "deeppink", linetype = "dashed") +
    geom_vline(xintercept = as.Date(date_decimal(decimal_date(dmy("20-03-2020")))), color = "grey", linetype = "dashed") +
    theme_minimal()
ggsave("mapJumpTraj.svg")

# final circular plot
chordData <- data %>%
    filter(
        id == "location.Feb_VIC_travelHistory_rep1" &
            state == 83140000
    ) %>%
    group_by(from, to) %>%
    summarise(value = n())


# arrow colours
arr_col <- data.frame(
    expand.grid(
        chordData$from, chordData$to,
        rep("black", times = length(chordData$to))
    )
)

# ordering state names
states <- sort(unique(c(chordData$to, chordData$from)))
grid_col <- viridis_pal(option = "plasma")(length(states))
names(grid_col) <- states

svg("mapJumpChordDigram.svg")
circlize::chordDiagram(
    chordData,
    grid.col = grid_col,
    order = states,
    directional = 1,
    direction.type = "arrows",
    link.arr.col = arr_col,
    link.arr.length = 0.2
)
dev.off()

# final proportion import export. NB, excluded for now
data %>%
    filter(to == "Australia") %>%
    mutate(combo = paste0(id, state)) %>%
    filter(combo %in% mapStates$combo) %>%
    mutate(jump = paste0(from, " -> ", to)) %>%
    mutate(
		date = as.Date(date_decimal((decimal_date(dmy("31-05-2020")) - time)))
	) %>%
    filter(id == "location.Feb_VIC_travelHistory_rep1") %>%
    mutate(direction = case_when(
        to == "Australia" ~ "import",
        from == "Australia" ~ "export",
        .default = NA
    )) %>%
    filter(direction %in% c("import", "export")) %>%
    ggplot(aes(x = date, fill = direction)) +
    geom_area(stat = "bin", binwidth = 7) +
    #geom_area(stat = "identity") +
    scale_fill_manual(values = viridis_pal(option = "plasma")(8), name = "") +
    scale_x_date(labels = scales::date_format("%b %Y")) +
    ylab("Weely proportion of migrations") +
    annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy("1-02-2020")))), xmax = as.Date(date_decimal(decimal_date(dmy("30-05-2020")))), ymin = 4000, ymax = 4200, alpha = .2) +
    annotate("text", x = as.Date(date_decimal(decimal_date(dmy("20-02-2020")))), y = 4100, label = "Travel ban from mainland China") +
    annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy("29-02-2020")))), xmax = as.Date(date_decimal(decimal_date(dmy("30-05-2020")))), ymin = 4200, ymax = 4400, alpha = .2) +
    annotate("text", x = as.Date(date_decimal(decimal_date(dmy("14-03-2020")))), y = 4300, label = "Travel ban from Iran") +
    annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy("05-03-2020")))), xmax = as.Date(date_decimal(decimal_date(dmy("30-05-2020")))), ymin = 4400, ymax = 4600, alpha = .2) +
    annotate("text", x = as.Date(date_decimal(decimal_date(dmy("23-03-2020")))), y = 4500, label = "Travel ban from South Korea") +
    annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy("11-03-2020")))), xmax = as.Date(date_decimal(decimal_date(dmy("30-05-2020")))), ymin = 4600, ymax = 4800, alpha = .2) +
    annotate("text", x = as.Date(date_decimal(decimal_date(dmy("25-03-2020")))), y = 4700, label = "Travel ban from Italy") +
    annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy("20-03-2020")))), xmax = as.Date(date_decimal(decimal_date(dmy("30-05-2020")))), ymin = 4800, ymax = 5000, alpha = .2) +
    annotate("text", x = as.Date(date_decimal(decimal_date(dmy("15-04-2020")))), y = 4900, label = "Borders closed to non-citizens and residents") +
    theme_minimal()
ggsave("mapJumpProp.svg")