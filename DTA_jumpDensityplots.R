library(ggplot2)
library(tidyverse)
library(viridis)
library(lubridate)

setwd("~/Downloads/April_DTA_results/")
jFiles <- dir(pattern='location.Feb_VIC_travelHistory_rep2jumpTimes.txt')

jHist <- lapply(jFiles, function(x) as_tibble(read.delim(x, header=T)))
colNames <- c(names(jHist[[1]]), 'id')

for (i in 1:length(jHist)){
	jHist[[i]] <- as_tibble(
		cbind(jHist[[i]], 
			rep(gsub(jFiles[i], pattern='location.Feb_VIC_travelHistory_rep2jumpTimes.txt', replacement=''), dim(jHist[[i]])[1])))
}

for (i in 1:length(jHist)){
	names(jHist[[i]])[5] <- 'dataset'
}


jTimes <- bind_rows(jHist, .id='id')

jTimes <- jTimes %>% mutate(type = paste0(from, ' -> ', to)) %>% filter(from=='Australia' | to=='Australia')

jTimesAus <- jTimes %>% mutate(type = paste0(from, ' -> ', to)) %>% filter(from=='Australia')
jTimesToAus <- jTimes %>% mutate(type = paste0(from, ' -> ', to)) %>% filter(to=='Australia')


pdf('Feb-thJumpFrequency.pdf', useDingbats=F, width=13, height=10)
	ggplot(jTimes, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=10) +
		scale_fill_manual(values=viridis_pal(option = "turbo")(16)) +
		facet_wrap(~dataset, scales='free') 
dev.off()

pdf('FebJumpFrequencyToAus.pdf', useDingbats=F, width=13, height=10)
ggplot(jTimesToAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', ) +
  scale_fill_manual(values=viridis_pal(option = "plasma")(8)) +
  facet_wrap(~dataset, scales='free')
dev.off()

pdf('FebJumpFrequencyFromAus.pdf', useDingbats=F, width=13, height=10)
ggplot(jTimesAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=7.5) +
  scale_fill_manual(values=viridis_pal(option = "plasma")(8)) +
  facet_wrap(~dataset, scales='free')
dev.off()


pdf('FebJumpProportion.pdf', useDingbats=F, width=13, height=10)
	ggplot(jTimes, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', position='fill', binwidth=5) +
		scale_fill_manual(values=viridis_pal(option = "turbo")(16)) +
		facet_wrap(~dataset, scales='free')
dev.off()


pdf('FebJumpProportionToAus.pdf', useDingbats=F, width=13, height=10)
ggplot(jTimesToAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', position='fill') +
  scale_fill_manual(values=viridis_pal(option = "plasma")(8)) +
  facet_wrap(~dataset, scales='free')
dev.off()

pdf('FebJumpProportionFromAus.pdf', useDingbats=F, width=13, height=10)
ggplot(jTimesAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', position='fill') +
  scale_fill_manual(values=viridis_pal(option = "plasma")(16)) +
  facet_wrap(~dataset, scales='free')
dev.off()

# just aus imports
impTimes <- jTimes %>% filter(to=='Australia')

pdf('FebImpProportionAus.pdf', useDingbats=F, width=13, height=10)
	ggplot(impTimes, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=10, position='fill') +
		scale_fill_manual(values=viridis(8)) +
		facet_wrap(~dataset, scales='free')
dev.off()


# looking at UK jump times
jTimesUK <- bind_rows(jHist, .id='id')

jTimesUK <- jTimesUK %>% mutate(type = paste0(from, ' -> ', to)) %>% filter(from=='UK')

# just aus imports
impTimes <- jTimes %>% filter(to=='Australia')

pdf('FebImpProportionUK.pdf', useDingbats=F, width=13, height=10)
	ggplot(jTimesUK, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=10, position='fill') +
		scale_fill_manual(values=viridis(8)) +
		facet_wrap(~dataset, scales='free')
dev.off()


### NEED Imp vs Exp, time axis ###

# condensing to Aus imp vs exp
# adding time axis. output of collect_times script is 'the time since the most recent sampling time'
# I'll add some random most recent date 31-05-2020

jTimes <- jTimes %>% mutate(type = paste0(from, ' -> ', to)) %>% filter(from=='Australia' | to=='Australia') %>% 
			mutate(bulkPattern = case_when(from=='Australia' ~ 'export', to=='Australia' ~ 'import'))

pdf('TEMPFebImpProportionBulkImpExp.pdf', useDingbats=F, width=13, height=10)
	


setEPS()
postscript("exportimport.eps")

ggplot(jTimes, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=bulkPattern)) + geom_area(stat='bin', binwidth=7.5, position='fill') +
		scale_fill_manual(values=alpha(viridis(2), 0.6)) +
		scale_x_date(labels = scales::date_format("%B %Y")) +
		xlab('Date') + 
		facet_wrap(~dataset, scales='free')	+ annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('1-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('26-05-2020')))), ymin = 1, ymax = 1.05, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('20-02-2020')))), y = 1.025, label = "Travel ban from mainland China")	+ annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('29-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('26-05-2020')))), ymin = 1.05, ymax = 1.1, alpha =.2)	+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('14-03-2020')))), y = 1.075, label = "Travel ban from Iran")+ annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('05-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('26-05-2020')))), ymin = 1.1, ymax = 1.15, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('23-03-2020')))), y = 1.125, label = "Travel ban from South Korea") + annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('11-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('26-05-2020')))), ymin =1.15, ymax = 1.2, alpha =.2) + annotate("text", x = as.Date(date_decimal(decimal_date(dmy('25-03-2020')))), y = 1.175, label = "Travel ban from Italy") + annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('20-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('26-05-2020')))), ymin = 1.2, ymax = 1.25, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('15-04-2020')))), y = 1.225, label="Borders closed to non-citizens and residents")
ggsave(file="impex.svg")  

dev.off()
  
  
	



	   pdf('FebJumpFrequencyToAus.eps', useDingbats=F, width=13, height=10)
	   ggplot(jTimesToAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type))+ geom_area(stat='bin', binwidth=10) +
	     scale_fill_manual(values=viridis_pal(option = "plasma")(8))+
	     scale_x_date(labels = scales::date_format("%B %Y")) +
	     xlab('Date') + 
	  facet_wrap(~dataset, scales='free')	+ 
	     annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('1-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 4000, ymax = 4200, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('20-02-2020')))), y = 4100, label = "Travel ban from mainland China")	+ 
	     annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('29-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 4200, ymax = 4400, alpha =.2)	+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('14-03-2020')))), y = 4300, label = "Travel ban from Iran")+ 
	     annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('05-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 4400, ymax = 4600, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('23-03-2020')))), y = 4500, label = "Travel ban from South Korea") + 
	     annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('11-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin =4600, ymax = 4800, alpha =.2) + annotate("text", x = as.Date(date_decimal(decimal_date(dmy('25-03-2020')))), y = 4700, label = "Travel ban from Italy") + 
	     annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('20-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin =4800, ymax = 5000, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('15-04-2020')))), y =4900, label="Borders closed to non-citizens and residents") + 
	     geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('1-02-2020')))), color = "purple", linetype="dashed") +
	     geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('29-02-2020')))), color = "darkmagenta", linetype="dashed")+
	     geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('05-03-2020')))), color = "darkblue", linetype="dashed")+
	     geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('11-03-2020')))), color = "deeppink", linetype="dashed")+
	     geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('20-03-2020')))), color = "grey", linetype="dashed")
	   dev.off()


	   
	   pdf('FebImpProportion.pdf', useDingbats=F, width=13, height=10)
	   ggplot(impTimes, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=0.1, position='fill') +
	     scale_fill_manual(values=viridis(8)) +
	     scale_x_date(labels = scales::date_format("%B %Y"))+
	     xlab('Date') + 
	     facet_wrap(~dataset, scales='free')	
	     #annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('1-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 2500, ymax = 2600, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('20-02-2020')))), y = 2550, label = "Travel ban from mainland China")	+ 
	     #annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('29-02-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 2600, ymax = 2700, alpha =.2)	+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('14-03-2020')))), y = 2650, label = "Travel ban from Iran")+ 
	     #annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('05-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 2700, ymax = 2800, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('23-03-2020')))), y = 2750, label = "Travel ban from South Korea") + 
	     #annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('11-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin =2800, ymax = 2900, alpha =.2) + annotate("text", x = as.Date(date_decimal(decimal_date(dmy('25-03-2020')))), y = 2850, label = "Travel ban from Italy") + 
	    #annotate("rect", xmin = as.Date(date_decimal(decimal_date(dmy('20-03-2020')))), xmax = as.Date(date_decimal(decimal_date(dmy('30-05-2020')))), ymin = 2900, ymax = 3000, alpha =.2)+ annotate("text", x = as.Date(date_decimal(decimal_date(dmy('15-04-2020')))), y = 2950, label="Borders closed to non-citizens and residents") + 
	     #geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('1-02-2020')))), color = "purple", linetype="dashed") +
	     #geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('29-02-2020')))), color = "darkmagenta", linetype="dashed")+
	     #geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('05-03-2020')))), color = "darkblue", linetype="dashed")+
	     #geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('11-03-2020')))), color = "deeppink", linetype="dashed")+
	     #geom_vline(xintercept  = as.Date(date_decimal(decimal_date(dmy('20-03-2020')))), color = "grey", linetype="dashed")
	   
	   dev.off()
	   
	   

	   pdf('FebJumpFrequencyToAus.pdf', useDingbats=F, width=13, height=10)
	   ggplot(jTimesToAus, aes(x=as.Date(date_decimal((decimal_date(dmy('31-05-2020'))-time))), fill=type)) + geom_area(stat='bin', binwidth=10) +
	     scale_fill_manual(values=viridis_pal(option = "plasma")(8)) +
	     facet_wrap(~dataset, scales='free')
	   dev.off()




















