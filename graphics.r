remove(list = ls())

# Check if the required libraries exist, if not install them 
required_lib =c("ggplot2","foreign","dplyr", "scales", "reshape2")

# Load required libraries
lapply(required_lib, require, character.only=T)

# Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

# Set working directory & load data
# setwd("U:/E3/Infra/")
setwd("C:/Users/t/Documents/GitHub/E3/Datain")

d <- read.csv("e3_data.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
df <- subset(d, subset=iso_3166==".")
df$const.rd <- round(df$construct_budget_plan_total/1000000, 0)


# Create graphics for awards using ggplot bar graphs
g <- ggplot(df, aes(x = reorder(factor(regnum), awards),
		y = awards, fill = "regnum")) + geom_bar(stat = "identity") +
		scale_fill_manual(values = redL)
pp <- g + coord_flip()+labs(x ="", title = "Total Awards", y = "(Award count)") +
	theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  
	print(pp)
  ggsave(pp, filename = paste("infra.awards", ".png"), width=7.5, height=5.5)

# Create graphics for award amounts
g <- ggplot(df, aes(x = reorder(factor(regnum), subawards),
		y = subawards, fill = "regnum")) + geom_bar(stat = "identity") +
		scale_fill_manual(values = redL)
pp <- g + coord_flip()+labs(x ="", title = "Total Sub-Awards", y = "(Sub-award count)") +
	theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  
	print(pp)
  ggsave(pp, filename = paste("infra.subawards", ".png"), width=7.5, height=5.5)

# Create graphics for constructionaward amounts
g <- ggplot(df, aes(x = reorder(factor(regnum), const.rd ),
		y = const.rd, fill = "regnum")) + geom_bar(stat = "identity") +
		scale_fill_manual(values = redL)
pp <- g + coord_flip()+labs(x ="", title = "Total Construction Budget (in $USD Millions)", y = "(Millions $USD)") +
	theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL)) + scale_y_continuous(labels = dollar)
	print(pp)
  ggsave(pp, filename = paste("infra.constbudget", ".png"), width=7.5, height=5.5)


# Create graphics for award amounts
g <- ggplot(df, aes(x = reorder(factor(regnum), conflict_status_awards ),
		y = conflict_status_awards, fill = "regnum")) + geom_bar(stat = "identity") +
		scale_fill_manual(values = redL)
pp <- g + coord_flip()+labs(x ="", title = "Awards in Conflict Areas", y = "Conflict award count") +
	theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))
	print(pp)
  ggsave(pp, filename = paste("infra.conflict", ".png"), width=7.5, height=5.5)


# Stacked bar graphs (melt data first)
df.tmp <- df[c(1, 11:15)]
names(df.tmp) <- c("Country", "buildings", "water", "tranport", "energy", "other")
df.melt <- melt(df.tmp, id.var = "Country")
names(df.melt) <- c("Country", "Type", "value")


# TODO: Figure out correct color scheme to use
g <- ggplot(df.melt, aes(x = reorder(factor(Country), value),
		y = value, fill = Type)) + geom_bar(stat = "identity")
pp <- g + coord_flip()+labs(x ="", title = "Award Types", y = "Award type count") +scale_fill_brewer(palette="RdYlGn") +
	theme(legend.position = "top", legend.title=element_blank(),
	 panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))
	print(pp)
  ggsave(pp, filename = paste("award.type", ".png"), width=7.5, height=5.5)

# Stacked bar graphs for remaining vars
df.tmp <- df[c(1, 16:24)]
#df.tmp$sum <- rowSums(df.tmp[2:10], na.rm = TRUE, dims = 1)
names(df.tmp) <- c("Country", "Cooperative Agreement", "Direct Contract", "Fixed Amount Reimbursement", "G-2-G Agreements",
	"Grants", "Host Country Awards", "Other", "PIO Grants", "USG Interagency Agreements")#, "Total")

#df.tmp$Country <- reorder(df.tmp$Country, rowSums(df.tmp[-1]) )
#levels(df.tmp$Country)

df.melt <- melt(df.tmp, id.var = "Country", na.rm = TRUE)
#df.melt$Country <- as.factor(df.melt$Country)

df.sub <- df.melt %>% group_by(Country) %>% mutate(sum = sum(value))

g <- ggplot(df.sub, aes(x = reorder(factor(Country), sum),
		y = value, fill = variable)) + geom_bar(stat = "identity")
pp <- g + coord_flip()+labs(x ="", title = "Award Types", y = "Award type count") +scale_fill_brewer(palette="RdYlGn") +
	theme(legend.position = "top", legend.title=element_blank(),
	 panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))
	print(pp)
  ggsave(pp, filename = paste("detail.type", ".png"), width=7.5, height=5.5)

# Now create similar graphs for all countrys by region
df.all <- subset(d, subset=iso_3166!=".")
df.all <- df.all[c(1, 4, 16:24)]

names(df.all) <- c("Country", "Region", "Cooperative Agreement", "Direct Contract", "Fixed Amount Reimbursement", "G-2-G Agreements",
                   "Grants", "Host Country Awards", "Other", "PIO Grants", "USG Interagency Agreements")#, "Total")

df.melt <- melt(df.all, id.var = c("Country", "Region"), na.rm = TRUE)

df.sub <- df.melt %>% group_by(Country, Region) %>% mutate(sum = sum(value))

g <- ggplot(df.sub, aes(x = reorder(factor(Country), sum),
                        y = value, fill = variable)) + geom_bar(stat = "identity")
pp <- g + coord_flip()+labs(x ="", title = "Award Types", y = "Award type count") +scale_fill_brewer(palette="RdYlGn") +
  theme(legend.position = "top", legend.title=element_blank(),
        panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
        axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
        axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
        axis.title.x = element_text(colour=dblueL, size=8),
        plot.title = element_text(lineheight=.8, colour = dblueL))
print(pp)
ggsave(pp, filename = paste("detail.type.all", ".png"), width=7.5, height=5.5)


