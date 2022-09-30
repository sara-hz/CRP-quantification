---
title: "CRP_quantification"
author: "Sara.H"
date: "09/29/2022"
#output: html_document
output: 
  pdf_document:
   keep_md: TRUE
---

STEP 1

---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Generated on: `r Sys.time()`

```{r }
library("tidyverse")
library("ggplot2")
library("rstatix")
library("ggpubr")
``` 

```{r }
CRPDataOrigFn <- "./Data/CRP-measurements.csv"
CRPDataParsedFn <- "./Data/CRP_tidied_complete.csv"
CRPDataDistLongFn <- "./Data/CRP_org_by_region.csv"
```

```{r }
# Read CRP data

CRPDataOrig <- read.csv2(CRPDataOrigFn, na.strings = "")
head(CRPDataOrig)

# Rename columns

colnames(CRPDataOrig)

CRPData <- CRPDataOrig %>% rename(file_name = Image, tiles_annotation = Parent, dist2inv_front_D = Distance.to.annotation.with.Desmoplastic.µm, dist2inv_front_R = Distance.to.annotation.with.Replacement.µm, CRP_mean = ROI..2.00.µm.per.pixel..DAB..Mean)
head(CRPData)

# Remove image file name extension
CRPData <- CRPData %>% mutate(file_name = str_replace(file_name, "(.ndpi)", ""))
head(CRPData)

CRPData$dist2inv_front_D <- as.numeric(CRPData$dist2inv_front_D)
CRPData$dist2inv_front_R <- as.numeric(CRPData$dist2inv_front_R)
CRPData$CRP_mean<- as.numeric(CRPData$CRP_mean)
typeof(CRPData$dist2inv_front_D)

```

```{r }
# Substitute in file name last occurrence of '-' by ':' to separate image name from markers
# Negative lookahead is used when you want to match something (Word1) not followed by something else (Word2):  Word1(?!.*Word2)
# We use this to find the last occurrence of a word in a string.
# https://www.xlayer.co.za/forum/viewtopic.php?id=105 , text in https://regex101.com/

CRPData <- CRPData %>% mutate(file_name = str_replace(file_name, "_(?!.*_)", ":")) 
unique(CRPData$file_name)

```

```{r }
# Separate file name into image name and classes
 CRPData <- CRPData %>% separate(file_name, into =  c("image", "marker"), sep = ":")
head(CRPData)

# Convert image and markers to factors
CRPData$image <- as.factor(CRPData$image)
CRPData$tiles_annotation <- as.factor(CRPData$tiles_annotation)
levels(CRPData$image)

``` 

```{r } 
# Group subclasses 
CRPData %>% group_by(tiles_annotation)

# Write tidied file to out
write.csv(CRPData, CRPDataParsedFn, row.names = F)

```

```{r }


#STEP 2


```

```{r }
#Separate the data frame by tiles_annotations into 2 data sets and add a column corresponding the tiles region before binding the data sets together
library(dplyr)

CRPDataInvFront_D<- CRPData %>% filter(tiles_annotation == "PLP D") %>% select(image, marker, dist2inv_front_D, CRP_mean) %>%                                              rename(distance = dist2inv_front_D) %>% mutate(region = "Desmoplastic") # For desmo rim

head(CRPDataInvFront_D)
max(CRPDataInvFront_D$distance, na.rm = TRUE)
range(CRPDataInvFront_D$distance, na.rm = TRUE)


CRPDataInvFront_R<- CRPData %>% filter(tiles_annotation == "PLP R") %>% select(image, marker, dist2inv_front_R, CRP_mean) %>%                                              rename(distance = dist2inv_front_R) %>% mutate(region = "Replacement") # For replacement

head(CRPDataInvFront_R)
max(CRPDataInvFront_R$distance, na.rm = TRUE)
range(CRPDataInvFront_R$distance, na.rm = TRUE)

CRPDataDistLong <- rbind(CRPDataInvFront_R, CRPDataInvFront_D) # Join both
head(CRPDataDistLong)

# Set values as numerics
CRPDataDistLong$distance <- as.numeric(CRPDataDistLong$distance)
CRPDataDistLong$CRP_mean<- as.numeric(CRPDataDistLong$CRP_mean)


# Write new file to out
write.csv(CRPDataDistLong, CRPDataDistLongFn, row.names = F)
```

```{r }

#Plot distribution of tile distances & tile stain intensities 


ggplot(data = CRPDataDistLong) + geom_histogram(mapping = aes(x = distance), binwidth = 25) + ggtitle("Distribution of tile distances")


ggplot(data = CRPDataDistLong) + geom_histogram(mapping = aes(x = CRP_mean), binwidth = 0.05) + xlim(-0.3, 1.2) +
  ggtitle("Distribution of tile stain intensities") 

```

```{r }

# Print range of distances in 50 intervals
seq(min(CRPDataDistLong$distance, na.rm = TRUE), max(CRPDataDistLong$distance, na.rm = TRUE), by = 50)

# Bin distances (by creating a categorical variable)
# Number of bins is 1 less than labels by seq, exclude label 0
x_bins <- seq(0, 900, by = 50) # From inv front to PLP
x_labels <- c()
for(cont in seq(1, length(x_bins)-1, by = 1)) {
  if(x_bins[cont] < 0 & x_bins[cont+1] < 0) {
    x_labels <- c(x_labels, paste0(x_bins[cont], "-",x_bins[cont+1]))
  } else {
    x_labels <- c(x_labels, paste0(x_bins[cont], "-",x_bins[cont+1]))
  }
}
print(x_labels)


CRPDataDistLong$dist_cat <- cut(CRPDataDistLong$distance, seq(0, 900, by = 50), labels = x_labels)
CRPDataDistLong <- CRPDataDistLong %>% relocate(dist_cat, .after = distance)
head(CRPDataDistLong)

```


```{r }
# Average intensities over bins
CRPDataDistAvg <- CRPDataDistLong %>% group_by(marker, region, dist_cat) %>% summarise(intensity_avg = mean(CRP_mean, na.rm = TRUE))
head(CRPDataDistAvg)

```


```{r }

#Plots

ggplot(data = subset(CRPDataDistAvg, !is.na(dist_cat)), aes(x = dist_cat, y = intensity_avg, group = region, colour = region)) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle(paste("CRP distribution from the invasion front_loess smoothened"))


ggplot(data = subset(CRPDataDistAvg, !is.na(dist_cat)), aes(x = dist_cat, y = intensity_avg, group = region, colour = region)) +
  geom_smooth(method = "gam", span = 0.2, se = FALSE) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle(paste("CRP distribution from the invasion front_ gam smoothened"))

```

```{r }
# Unpaired Wilcox-test

res <- wilcox.test(intensity_avg ~ region, data = CRPDataDistAvg,
                   exact = FALSE)
res

format(res$p.value, scientific=FALSE);
```

```{r }

# Plot intensity by region 

library("ggpubr")
ggboxplot(CRPDataDistAvg, x = "region", y = "intensity_avg", 
          color = "region", palette = c("#00AFBB", "#E7B800"),
          ylab = "CRP_intensity_avg", xlab = "") + ggtitle(res <- wilcox.test(intensity_avg ~ region, data = CRPDataDistAvg,
                   exact = FALSE) , format(res$p.value,  scientific=FALSE ))

```

```{r }


ggplot(subset(CRPDataDistAvg, !is.na(dist_cat)), aes(x = dist_cat, y = intensity_avg)) + 
geom_boxplot(aes(fill = region), binwidth = 25, alpha = .2) +
geom_line(aes(group = region)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
geom_point(aes(col = region), size = 1) + 
facet_wrap(~ region)

```

