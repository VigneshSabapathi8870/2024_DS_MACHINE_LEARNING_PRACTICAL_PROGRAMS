---
title: "R Notebook"
output: html_notebook
---

```{r}
library(tidyverse)
library(ISLR)
library(ISLR2)
```
```{r}
data(Auto)
attach(Auto)

```
```{r}
dim(Auto)
```
```{r}
?Auto
```


```{r}
summary(Auto)
```
```{r}
auto1=na.omit(Auto)
```


```{r}
Auto
dim(auto1)
```


```{r}
plot(cylinders,mpg)
```


```{r}
ggplot(Auto,mapping = aes(cylinders,mpg))+geom_point(color="red")
```


```{r}
data(College)
```


```{r}


view(College)
```


```{r}
dim(College)
```
```{r}
summary(College)
```


```{r}
str(College)
```


```{r}
?College
```


```{r}
attach(College)
ggplot(data=College,mapping = aes(Outstate,Private))+geom_boxplot()
plot(Outstate,Private,type = "boxplot")
```
```{r}

```



