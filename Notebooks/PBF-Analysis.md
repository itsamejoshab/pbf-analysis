
# **Philaldelphia Bail Fund**

<span style="color: grey;">August 5, 2020</span>

[GitHub](https://github.com/CodeForPhilly/pbf-analysis)  
[Website](https://www.phillybailfund.org/)

-----

#### ***Table of Contents***

  - [Introduction](#**Introduction**)
  - [Data Cleaning](#**Data%20Cleaning**)
  - [Text Preprocessing](#**Text%20Preprocessing**)
  - [Predictive Modeling](#**Predictive%20Modeling**)
  - [Analysis](#**Analysis**)
  - [Ideas for Further Analysis](#**Ideas%20for%20Further%20Analysis**)

-----

### <a id="**Introduction**"></a>**Introduction**

\[\]

### <a id="**Data Cleaning**"></a>**Data Cleaning**

``` r
library(dplyr)
library(corrplot)
library(Metrics)
library(gbm)
library(pdp)
library(ggplot2)
library(stringr)
library(tm) 
library(textstem)
library(tidytext)
library(wordcloud)
```

#### A1. Import data

``` r
url <- "https://raw.githubusercontent.com/CodeForPhilly/pbf-analysis/master/Data/0c_distinct_dockets.csv"
data <- read.csv(url)
head(data, 10)
```

    ##      id age                address         docket_number          filing_date
    ## 1  3909  27 Philadelphia, PA 19141 MC-51-CR-0011746-2020 2020-06-16T00:37:00Z
    ## 2  4538  44 Philadelphia, PA 19124 MC-51-CR-0011747-2020 2020-06-16T00:41:00Z
    ## 3   120  24 Philadelphia, PA 19142 MC-51-CR-0011743-2020 2020-06-16T00:52:00Z
    ## 4   120  24 Philadelphia, PA 19142 MC-51-CR-0011744-2020 2020-06-16T00:52:00Z
    ## 5   120  24 Philadelphia, PA 19142 MC-51-CR-0011745-2020 2020-06-16T00:52:00Z
    ## 6   291  32 Philadelphia, PA 19440 MC-51-CR-0011748-2020 2020-06-16T01:03:00Z
    ## 7   291  32 Philadelphia, PA 19440 MC-51-CR-0011749-2020 2020-06-16T01:03:00Z
    ## 8   291  32 Philadelphia, PA 19440 MC-51-CR-0011750-2020 2020-06-16T01:03:00Z
    ## 9  2396  51 Philadelphia, PA 19145 MC-51-CR-0011751-2020 2020-06-16T01:08:00Z
    ## 10 2008  41 Philadelphia, PA 19136 MC-51-CR-0011752-2020 2020-06-16T01:12:00Z
    ##                                                                     charge
    ## 1                             DUI: Gen Imp/Inc of Driving Safely - 1st Off
    ## 2                               Verify Address or Photographed as Required
    ## 3                                                        Criminal Mischief
    ## 4                                                        Criminal Mischief
    ## 5                                                        Criminal Mischief
    ## 6                             Contempt For Violation of Order or Agreement
    ## 7  Burglary - Overnight Accommodations Person Present, Bodily Injury Crime
    ## 8  Burglary - Overnight Accommodations Person Present, Bodily Injury Crime
    ## 9                                                           Simple Assault
    ## 10                                          Poss Instrument Of Crime W/Int
    ##                          represented_by bail_type bail_status bail_amount
    ## 1  Defender Association of Philadelphia    Posted         ROR           0
    ## 2  Defender Association of Philadelphia       Set    Monetary       50000
    ## 3  Defender Association of Philadelphia    Posted         ROR           0
    ## 4  Defender Association of Philadelphia    Posted         ROR           0
    ## 5  Defender Association of Philadelphia    Posted         ROR           0
    ## 6  Defender Association of Philadelphia       Set    Monetary       50000
    ## 7  Defender Association of Philadelphia       Set    Monetary       75000
    ## 8  Defender Association of Philadelphia       Set    Monetary       75000
    ## 9  Defender Association of Philadelphia    Posted   Unsecured       25000
    ## 10 Defender Association of Philadelphia    Posted   Unsecured       25000
    ##    outstanding_bail_amount
    ## 1                        0
    ## 2                        0
    ## 3                        0
    ## 4                        0
    ## 5                        0
    ## 6                        0
    ## 7                        0
    ## 8                        0
    ## 9                        0
    ## 10                       0

#### A2. Delete rows where bail\_type = ‘Denied’ and bail\_status = “ROR” or “Nonmonetary”

``` r
data <- data %>% mutate(bail_status = as.character(bail_status),
                        bail_type = as.character(bail_type)) %>% 
  filter(!bail_status %in% c("Nonmonetary", "ROR"), 
         !bail_type %in% c("Denied"))
```

#### A3. Create hour of day and day of week from filing\_date

``` r
data$date <- as.Date(data$filing_date)
data$day_of_week <- as.factor(weekdays(data$date))
data$time <- as.numeric(substr(data$filing_date, 12, 13))
data$bail_status <- as.factor(data$bail_status)
```

#### A4. Impute age with average of age

``` r
avg.age <- mean(data[-which(is.na(data)),]$age)
data <- data %>% mutate(age = ifelse(is.na(age) == TRUE, avg.age, age))

ggplot(data, aes(x=date, y=bail_amount)) +
  geom_line( color="steelblue") + 
  xlab("")
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### A5. Keep = bail\_amount, charge, day of week, hour of day, age, represented\_by, address

``` r
data.gb <- data[, c(2, 6, 7, 10, 13, 14)] 
```

## <a id="**Text Preprocessing**"></a>**Text Preprocessing**

#### B1. Clean text

``` r
data$charge = tolower(data$charge)

new_stopwords <- c(stopwords('en'), "criminal", "crim", "cause", "intent", "int", "attempt")
data$charge = gsub(paste(new_stopwords, collapse = '\\b|\\b'), '', data$charge)
data$charge = gsub('[[:digit:]]', '', data$charge)
data$charge = gsub('[[:punct:]]', '', data$charge)
data$charge = gsub('\\s+',' ', data$charge)
```

#### B2. Group charges - reduce from 136 to 97 levels

``` r
#data$charge <- as.character(data$charge)
data$charge <- ifelse(str_detect(data$charge, "burglary"), "burglary",
                  ifelse(str_detect(data$charge, "robbery"), "robbery", 
                      ifelse(str_detect(data$charge, "terrorize"), "terrorist",
                          ifelse(str_detect(data$charge, "murder"), "murder", 
                              ifelse(str_detect(data$charge, "statutory"), "statutory sexual assault",
                                  ifelse(str_detect(data$charge, "harassment"), "harassment",
                                        ifelse(str_detect(data$charge, "agg"), "aggravated assault",
                                            ifelse(str_detect(data$charge, "theft"), "theft",
                                                ifelse(str_detect(data$charge, "asslt"), "indecent assault",
                                                    ifelse(str_detect(data$charge, "firearm"), "firearm possession/delivery",
                                                        ifelse(str_detect(data$charge, "rape"), "rape", data$charge)))))))))))          

#data$charge = lemmatize_strings(data$charge)
data$charge <- as.factor(data$charge)
str(data)
```

    ## 'data.frame':    4097 obs. of  14 variables:
    ##  $ id                     : int  4538 291 291 291 2396 2008 1244 1003 3840 3840 ...
    ##  $ age                    : num  44 32 32 32 51 41 44 20 39 39 ...
    ##  $ address                : Factor w/ 346 levels "Abington, PA 19001",..: 226 263 263 263 248 239 223 243 250 250 ...
    ##  $ docket_number          : Factor w/ 5654 levels "MC-51-CR-0005549-2020",..: 5612 5613 5614 5615 5616 5617 5618 5619 5620 5621 ...
    ##  $ filing_date            : Factor w/ 5022 levels "2020-02-29T00:42:00Z",..: 4986 4988 4988 4988 4989 4990 4991 4992 4993 4993 ...
    ##  $ charge                 : Factor w/ 82 levels " arsondanger death bodily inj",..: 81 25 21 21 68 58 25 37 75 10 ...
    ##  $ represented_by         : Factor w/ 72 levels "Alboum, Samuel",..: 14 14 14 14 14 14 14 14 14 14 ...
    ##  $ bail_type              : chr  "Set" "Set" "Set" "Set" ...
    ##  $ bail_status            : Factor w/ 2 levels "Monetary","Unsecured": 1 1 1 1 2 2 2 1 2 1 ...
    ##  $ bail_amount            : int  50000 50000 75000 75000 25000 25000 75000 200000 10000 15000 ...
    ##  $ outstanding_bail_amount: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ date                   : Date, format: "2020-06-16" "2020-06-16" ...
    ##  $ day_of_week            : Factor w/ 7 levels "Friday","Monday",..: 6 6 6 6 6 6 6 6 6 6 ...
    ##  $ time                   : num  0 1 1 1 1 1 4 4 4 4 ...

#### B3. Create tdm

``` r
corp <- Corpus(VectorSource(data$charge))
tdm <- TermDocumentMatrix(corp, control = list(wordLengths = c(1, Inf)))
tidy_frame <- tidy(tdm)
```

#### D7. Wordcloud

``` r
cloud_data <- tidy_frame %>% group_by(term) %>% summarise(counts = sum(count))
head(cloud_data %>% arrange(-counts))  # these are the most common words
```

    ## # A tibble: 6 x 2
    ##   term        counts
    ##   <chr>        <dbl>
    ## 1 manufacture   1332
    ## 2 assault        805
    ## 3 possession     670
    ## 4 delivery       667
    ## 5 deliver        666
    ## 6 burglary       650

``` r
wordcloud(words=cloud_data$term, freq=cloud_data$counts, random.order=FALSE, colors=brewer.pal(7, "Greens"), max.words = 20)
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## <a id="**Predictive Modeling**"></a>**Predictive Modeling**

#### C1. Split the dataset randomly into a training and a test set

``` r
data.gb <- data[, c(2, 6, 7, 10, 13, 14)] 

set.seed(123) 
train_index <- sample(nrow(data.gb), size = round(0.75 * nrow(data.gb)), replace = FALSE)
train <- data.gb[train_index,]
test <- data.gb[-train_index,]
```

#### C2. Gradient Boosting Machine - Model tuning by grid search; Best MAPE chosen

``` r
set.seed(123)
gb_parameters <- data.frame(sample_size = round(runif(10,0.1,0.6), 1),
                            shrink = round(runif(10,0.001,0.008), 3))

mape_gb <- data.frame(mape_train_gb = numeric(), mape_test_gb = numeric())

for(paracomb_gb in 1:nrow(gb_parameters)){
  gradient_boosting <- gbm(bail_amount ~ ., data = train, 
                           distribution = "gaussian",
                           n.trees = 500,
                           interaction.depth = 6,
                           n.minobsinnode = 5, 
                           cv.folds = 3,
                           bag.fraction = gb_parameters[paracomb_gb,'sample_size'], 
                           shrinkage = gb_parameters[paracomb_gb,'shrink']
                           )
  
  pred_train_gb <- predict(gradient_boosting, train, n.trees = 500)
  mape_train_gb <- mape(pred_train_gb, train$bail_amount)
  
  pred_test_gb <- predict(gradient_boosting, test, n.trees = 500)
  mape_test_gb <- mape(pred_test_gb, test$bail_amount)#

  mape_gb[paracomb_gb, ] <- c(mape_train_gb, mape_test_gb)

  
}

cbind(gb_parameters, mape_gb)
```

    ##    sample_size shrink mape_train_gb mape_test_gb
    ## 1          0.2  0.008     0.8240884    0.8399662
    ## 2          0.5  0.004     0.8066001    0.8180591
    ## 3          0.3  0.006     0.8059342    0.8265253
    ## 4          0.5  0.005     0.8052536    0.8198507
    ## 5          0.6  0.002     0.8110181    0.8256825
    ## 6          0.1  0.007     0.8395823    0.8745898
    ## 7          0.4  0.003     0.8056377    0.8198688
    ## 8          0.5  0.001     0.8579976    0.8660399
    ## 9          0.4  0.003     0.8049278    0.8179743
    ## 10         0.3  0.008     0.8165624    0.8357909

``` r
set.seed(123)
best_gradient_boosting <- gbm(bail_amount ~ ., data = train, 
                              distribution = "gaussian",
                              n.trees = 500,
                              interaction.depth = 6,
                              n.minobsinnode = 5, 
                              cv.folds = 3,
                              bag.fraction = 0.4 , 
                              shrinkage = 0.003
)

gbm.perf(best_gradient_boosting, method = "cv")
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    ## [1] 345

``` r
pred_train_bestgb <- predict(best_gradient_boosting, train, n.trees = 500)
mape(pred_train_bestgb, train$bail_amount)
```

    ## [1] 0.8067403

``` r
pred_test_bestgb <- predict(best_gradient_boosting, test, n.trees = 500)
mape(pred_test_bestgb, test$bail_amount)
```

    ## [1] 0.8208372

## <a id="**Analysis**"></a>**Analysis**

#### D1. Correlation

``` r
correlation <- cor(data.gb[,c(1,4,6)])
corrplot(correlation, type = "lower", col = c("pink", "lightblue"), addCoef.col = "black", tl.col = "black")
```

<img src="PBF-Analysis_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

#### D2. GBM - Relative Importance and Partial Dependence Plot of each independent variable

``` r
summary(best_gradient_boosting)
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

    ##                           var   rel.inf
    ## charge                 charge 57.050884
    ## time                     time 17.149800
    ## day_of_week       day_of_week 12.486605
    ## represented_by represented_by  7.930062
    ## age                       age  5.382649

``` r
x_ax = 1:length(pred_test_bestgb)
plot(x_ax, pred_test_bestgb, col="blue", pch=20, cex=.9)
lines(x_ax, pred_test_bestgb, col="red", pch=20, cex=.9) 
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
plot(best_gradient_boosting, i = "age")
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
plot(best_gradient_boosting, i = "time")
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
plot(best_gradient_boosting, i = "age")
```

![](PBF-Analysis_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->

## <a id="**Ideas for Further Analysis**"></a>**Ideas for Further Analysis**

E1. Compare location and median income/crime rate/race distribution to
see if it has an impact on the bail\_amount  
E2. Compare bail\_amount between public, private, and no defender  
E3. Size/color map of bail\_amount  
E4. Time series chart of bail\_amount; cross check with BLM movement  
E5. Most common charges
