---
title: "Untitled"
author: "Sam Pritchard"
date: "December 20, 2017"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}

data17 <- read.csv("Detailed Sales Report 17.csv", stringsAsFactors = FALSE)
data16 <- read.csv("Detailed Sales Report 16.csv", stringsAsFactors = FALSE)

depts17 <- unique(data17$Department)
depts17 <- trimws(depts17)
depts17 <- unique(depts17)

print("These are the Departments in 2017")

print(depts17)

categories17 <- data17$Category
categories17 <- unique(trimws(categories17))
print("These are the Categories in 2017")
print(categories17)

sales.by.dept17 <- aggregate(Price ~ Department, data17, sum)
sales.by.dept17 <- arrange(sales.by.dept17, desc(Price))
print("Depts ranked by Sales 2017")
print(sales.by.dept17)
sales.by.dept17a <- sales.by.dept17
sales.by.dept17a <- sales.by.dept17a[,1:2]
colnames(sales.by.dept17a) <- c("Department", "2017")

data16$Department <- trimws(data16$Department)
departments16 <- unique(data16$Department)
sales.by.dept16 <- aggregate(Price ~ Department, data16, sum)
sales.by.dept16a <- sales.by.dept16
sales.by.dept16a <- sales.by.dept16a[,1:2]
colnames(sales.by.dept16a) <- c("Department", "2016")
sales.by.dept2 <- merge(sales.by.dept17a, sales.by.dept16a, all.x = TRUE, all.y = TRUE)

dept.plot <- ggplot(data = sales.by.dept) +
  geom_segment(aes(x=1, xend=2, y=`2016`, yend=`2017`)) + 
  geom_text(aes(label=Department), y=`2016`, x=1)
print(dept.plot)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
