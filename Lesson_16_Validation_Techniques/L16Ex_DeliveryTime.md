## Example 11.2

#### Perform data housekeeping - upload, name columns, display to make sure it reads properly, etc.

``` r
knitr::opts_chunk$set(echo = TRUE)

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-14.0.1') # for 64-bit version
#library(rJava)

library("xlsx") # Needed to read data
```

    ## Warning: package 'xlsx' was built under R version 4.0.4

``` r
# Import data
Lex16_2 <- read.xlsx("data-ex-11-2.xlsx", sheetIndex = 1, sheetName=NULL, rowIndex=NULL, startRow=NULL, endRow=NULL, colIndex= NULL, as.data.frame=TRUE, header=TRUE, colClasses=NA, keepFormulas=FALSE, encoding="unknown")

# Give labels to data columns
names(Lex16_2) <- c("Obs", "City", "time", "cases", "distance")
attach(Lex16_2)

# Output data to make sure it reads properly
Lex16_2 
```

    ##    Obs       City  time cases distance
    ## 1    1       <NA> 16.68     7      560
    ## 2    2       <NA> 11.50     3      220
    ## 3    3       <NA> 12.03     3      340
    ## 4    4       <NA> 14.88     4       80
    ## 5    5       <NA> 13.75     6      150
    ## 6    6       <NA> 18.11     7      330
    ## 7    7       <NA>  8.00     2      110
    ## 8    8       <NA> 17.83     7      210
    ## 9    9       <NA> 79.24    30     1460
    ## 10  10       <NA> 21.50     5      605
    ## 11  11       <NA> 40.33    16      688
    ## 12  12       <NA> 21.00    10      215
    ## 13  13       <NA> 13.50     4      255
    ## 14  14       <NA> 19.75     6      462
    ## 15  15       <NA> 24.00     9      448
    ## 16  16       <NA> 29.00    10      776
    ## 17  17       <NA> 15.35     6      200
    ## 18  18       <NA> 19.00     7      132
    ## 19  19       <NA>  9.50     3       36
    ## 20  20       <NA> 35.10    17      770
    ## 21  21       <NA> 17.90    10      140
    ## 22  22       <NA> 52.32    26      810
    ## 23  23       <NA> 18.75     9      450
    ## 24  24       <NA> 19.83     8      635
    ## 25  25       <NA> 10.75     4      150
    ## 26  26  San Diego 51.00    22      905
    ## 27  27  San Diego 16.80     7      520
    ## 28  28     Boston 26.16    15      290
    ## 29  29     Boston 19.90     5      500
    ## 30  30     Boston 24.00     6     1000
    ## 31  31     Boston 18.55     6      225
    ## 32  32     Boston 31.93    10      775
    ## 33  33     Boston  6.95     4      212
    ## 34  34     Austin  7.00     1      144
    ## 35  35     Austin 14.00     3      126
    ## 36  36     Austin 37.03    12      655
    ## 37  37 Louisville 18.62    10      420
    ## 38  38 Louisville 15.10     7      150
    ## 39  39 Louisville 24.38     8      360
    ## 40  40 Louisville 64.75    32     1530

``` r
# Output data dimensions
dim(Lex16_2)
```

    ## [1] 40  5

``` r
### Example 11.2 (375-376) ###
# Distinguish between original data and new data
dfnew <- subset(Lex16_2, Obs > 25)
dfold <- subset(Lex16_2, Obs <= 25)

# Create model using original data
model.old <- lm(dfold$time ~ dfold$cases + dfold$distance)

summary(model.old)
```

    ## 
    ## Call:
    ## lm(formula = dfold$time ~ dfold$cases + dfold$distance)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.7880 -0.6629  0.4364  1.1566  7.4197 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2.341231   1.096730   2.135 0.044170 *  
    ## dfold$cases    1.615907   0.170735   9.464 3.25e-09 ***
    ## dfold$distance 0.014385   0.003613   3.981 0.000631 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.259 on 22 degrees of freedom
    ## Multiple R-squared:  0.9596, Adjusted R-squared:  0.9559 
    ## F-statistic: 261.2 on 2 and 22 DF,  p-value: 4.687e-16

``` r
anova(model.old)
```

    ## Analysis of Variance Table
    ## 
    ## Response: dfold$time
    ##                Df Sum Sq Mean Sq F value    Pr(>F)    
    ## dfold$cases     1 5382.4  5382.4 506.619 < 2.2e-16 ***
    ## dfold$distance  1  168.4   168.4  15.851 0.0006312 ***
    ## Residuals      22  233.7    10.6                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Predict values in new dataset using original model
y_new_hat <- model.old$coefficients[1] +
  model.old$coefficients[2]*dfnew$cases +
  model.old$coefficients[3]*dfnew$distance

the_diff <- dfnew$time - y_new_hat
```

## Reproduce Table 11.2 on p. 376

``` r
library(e1071)
library(xtable)

table_11pt2 <- data.frame(dfnew$Obs,
                                 dfnew$City,
                                 dfnew$cases,
                                 dfnew$distance,
                                 dfnew$time,
                                 y_new_hat,
                                 the_diff)
                                 
out <- table_11pt2
colnames(out) <- c("Observation",
                   "City",
                   "Cases, $x_1$",
                   "Distance, $x_2$",
                   "Observed Time, $y$",
                   "Least Squares Fit, $\\hat{y}$",
                   "Least Squares Fit, $y-\\hat{y}$")

tab <- (xtable(out,digits=c(0,0,NA,0,0,2,4,4)))
print(tab, type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Mar 17 19:19:49 2021 -->
<table border="1">
<tr>
<th>
</th>
<th>
Observation
</th>
<th>
City
</th>
<th>
Cases, *x*<sub>1</sub>
</th>
<th>
Distance, *x*<sub>2</sub>
</th>
<th>
Observed Time, *y*
</th>
<th>
Least Squares Fit, *ŷ*
</th>
<th>
Least Squares Fit, *y* − *ŷ*
</th>
</tr>
<tr>
<td align="right">
1
</td>
<td align="right">
26
</td>
<td>
San Diego
</td>
<td align="right">
22
</td>
<td align="right">
905
</td>
<td align="right">
51.00
</td>
<td align="right">
50.9095
</td>
<td align="right">
0.0905
</td>
</tr>
<tr>
<td align="right">
2
</td>
<td align="right">
27
</td>
<td>
San Diego
</td>
<td align="right">
7
</td>
<td align="right">
520
</td>
<td align="right">
16.80
</td>
<td align="right">
21.1327
</td>
<td align="right">
-4.3327
</td>
</tr>
<tr>
<td align="right">
3
</td>
<td align="right">
28
</td>
<td>
Boston
</td>
<td align="right">
15
</td>
<td align="right">
290
</td>
<td align="right">
26.16
</td>
<td align="right">
30.7514
</td>
<td align="right">
-4.5914
</td>
</tr>
<tr>
<td align="right">
4
</td>
<td align="right">
29
</td>
<td>
Boston
</td>
<td align="right">
5
</td>
<td align="right">
500
</td>
<td align="right">
19.90
</td>
<td align="right">
17.6132
</td>
<td align="right">
2.2868
</td>
</tr>
<tr>
<td align="right">
5
</td>
<td align="right">
30
</td>
<td>
Boston
</td>
<td align="right">
6
</td>
<td align="right">
1000
</td>
<td align="right">
24.00
</td>
<td align="right">
26.4215
</td>
<td align="right">
-2.4215
</td>
</tr>
<tr>
<td align="right">
6
</td>
<td align="right">
31
</td>
<td>
Boston
</td>
<td align="right">
6
</td>
<td align="right">
225
</td>
<td align="right">
18.55
</td>
<td align="right">
15.2733
</td>
<td align="right">
3.2767
</td>
</tr>
<tr>
<td align="right">
7
</td>
<td align="right">
32
</td>
<td>
Boston
</td>
<td align="right">
10
</td>
<td align="right">
775
</td>
<td align="right">
31.93
</td>
<td align="right">
29.6485
</td>
<td align="right">
2.2815
</td>
</tr>
<tr>
<td align="right">
8
</td>
<td align="right">
33
</td>
<td>
Boston
</td>
<td align="right">
4
</td>
<td align="right">
212
</td>
<td align="right">
6.95
</td>
<td align="right">
11.8544
</td>
<td align="right">
-4.9044
</td>
</tr>
<tr>
<td align="right">
9
</td>
<td align="right">
34
</td>
<td>
Austin
</td>
<td align="right">
1
</td>
<td align="right">
144
</td>
<td align="right">
7.00
</td>
<td align="right">
6.0286
</td>
<td align="right">
0.9714
</td>
</tr>
<tr>
<td align="right">
10
</td>
<td align="right">
35
</td>
<td>
Austin
</td>
<td align="right">
3
</td>
<td align="right">
126
</td>
<td align="right">
14.00
</td>
<td align="right">
9.0014
</td>
<td align="right">
4.9986
</td>
</tr>
<tr>
<td align="right">
11
</td>
<td align="right">
36
</td>
<td>
Austin
</td>
<td align="right">
12
</td>
<td align="right">
655
</td>
<td align="right">
37.03
</td>
<td align="right">
31.1542
</td>
<td align="right">
5.8758
</td>
</tr>
<tr>
<td align="right">
12
</td>
<td align="right">
37
</td>
<td>
Louisville
</td>
<td align="right">
10
</td>
<td align="right">
420
</td>
<td align="right">
18.62
</td>
<td align="right">
24.5419
</td>
<td align="right">
-5.9219
</td>
</tr>
<tr>
<td align="right">
13
</td>
<td align="right">
38
</td>
<td>
Louisville
</td>
<td align="right">
7
</td>
<td align="right">
150
</td>
<td align="right">
15.10
</td>
<td align="right">
15.8103
</td>
<td align="right">
-0.7103
</td>
</tr>
<tr>
<td align="right">
14
</td>
<td align="right">
39
</td>
<td>
Louisville
</td>
<td align="right">
8
</td>
<td align="right">
360
</td>
<td align="right">
24.38
</td>
<td align="right">
20.4470
</td>
<td align="right">
3.9330
</td>
</tr>
<tr>
<td align="right">
15
</td>
<td align="right">
40
</td>
<td>
Louisville
</td>
<td align="right">
32
</td>
<td align="right">
1530
</td>
<td align="right">
64.75
</td>
<td align="right">
76.0590
</td>
<td align="right">
-11.3090
</td>
</tr>
</table>

## Example 11.3

## Reproduce Table 11.6 on p. 385

``` r
### Example 11.3 (p. 380-385) ###
# Import new data, defining which data points are in Estimation set and which are in Prediction set

# Import data
Lex16_3 <- read.xlsx("data-ex-11-3.xlsx", sheetIndex = 1, sheetName=NULL, rowIndex=NULL, startRow=NULL, endRow=NULL, colIndex= NULL, as.data.frame=TRUE, header=TRUE, colClasses=NA, keepFormulas=FALSE, encoding="unknown")


# Give labels to data columns
names(Lex16_3) <- c("Obs", "City", "time", "cases", "distance","EorP")
attach(Lex16_3)
```

    ## The following objects are masked from Lex16_2:
    ## 
    ##     cases, City, distance, Obs, time

``` r
# Output data to make sure it reads properly
the_data <- data.frame(Lex16_3$Obs,
                       Lex16_3$City,
                       Lex16_3$time,
                       Lex16_3$cases,
                       Lex16_3$distance,
                       Lex16_3$EorP)
                                 
out <- the_data
colnames(out) <- c("Observation, $i$",
                   "City",
                   "Delivery TIme, $y$",
                   "Cases, $x_1$",
                   "Distance, $x_2$",
                   "Estimation (E) or Prediction (P) Data Set")

tab <- (xtable(out,digits=c(0,0,NA,2,0,0,NA)))
print(tab, type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Mar 17 19:19:49 2021 -->
<table border="1">
<tr>
<th>
</th>
<th>
Observation, *i*
</th>
<th>
City
</th>
<th>
Delivery TIme, *y*
</th>
<th>
Cases, *x*<sub>1</sub>
</th>
<th>
Distance, *x*<sub>2</sub>
</th>
<th>
Estimation (E) or Prediction (P) Data Set
</th>
</tr>
<tr>
<td align="right">
1
</td>
<td align="right">
1
</td>
<td>
</td>
<td align="right">
16.68
</td>
<td align="right">
7
</td>
<td align="right">
560
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
2
</td>
<td align="right">
2
</td>
<td>
</td>
<td align="right">
11.50
</td>
<td align="right">
3
</td>
<td align="right">
220
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
3
</td>
<td align="right">
3
</td>
<td>
</td>
<td align="right">
12.03
</td>
<td align="right">
3
</td>
<td align="right">
340
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
4
</td>
<td align="right">
4
</td>
<td>
</td>
<td align="right">
14.88
</td>
<td align="right">
4
</td>
<td align="right">
80
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
5
</td>
<td align="right">
5
</td>
<td>
</td>
<td align="right">
13.75
</td>
<td align="right">
6
</td>
<td align="right">
150
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
6
</td>
<td align="right">
6
</td>
<td>
</td>
<td align="right">
18.11
</td>
<td align="right">
7
</td>
<td align="right">
330
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
7
</td>
<td align="right">
7
</td>
<td>
</td>
<td align="right">
8.00
</td>
<td align="right">
2
</td>
<td align="right">
110
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
8
</td>
<td align="right">
8
</td>
<td>
</td>
<td align="right">
17.83
</td>
<td align="right">
7
</td>
<td align="right">
210
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
9
</td>
<td align="right">
9
</td>
<td>
</td>
<td align="right">
79.24
</td>
<td align="right">
30
</td>
<td align="right">
1460
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
10
</td>
<td align="right">
10
</td>
<td>
</td>
<td align="right">
21.50
</td>
<td align="right">
5
</td>
<td align="right">
605
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
11
</td>
<td align="right">
11
</td>
<td>
</td>
<td align="right">
40.33
</td>
<td align="right">
16
</td>
<td align="right">
688
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
12
</td>
<td align="right">
12
</td>
<td>
</td>
<td align="right">
21.00
</td>
<td align="right">
10
</td>
<td align="right">
215
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
13
</td>
<td align="right">
13
</td>
<td>
</td>
<td align="right">
13.50
</td>
<td align="right">
4
</td>
<td align="right">
255
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
14
</td>
<td align="right">
14
</td>
<td>
</td>
<td align="right">
19.75
</td>
<td align="right">
6
</td>
<td align="right">
462
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
15
</td>
<td align="right">
15
</td>
<td>
</td>
<td align="right">
24.00
</td>
<td align="right">
9
</td>
<td align="right">
448
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
16
</td>
<td align="right">
16
</td>
<td>
</td>
<td align="right">
29.00
</td>
<td align="right">
10
</td>
<td align="right">
776
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
17
</td>
<td align="right">
17
</td>
<td>
</td>
<td align="right">
15.35
</td>
<td align="right">
6
</td>
<td align="right">
200
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
18
</td>
<td align="right">
18
</td>
<td>
</td>
<td align="right">
19.00
</td>
<td align="right">
7
</td>
<td align="right">
132
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
19
</td>
<td align="right">
19
</td>
<td>
</td>
<td align="right">
9.50
</td>
<td align="right">
3
</td>
<td align="right">
36
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
20
</td>
<td align="right">
20
</td>
<td>
</td>
<td align="right">
35.10
</td>
<td align="right">
17
</td>
<td align="right">
770
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
21
</td>
<td align="right">
21
</td>
<td>
</td>
<td align="right">
17.90
</td>
<td align="right">
10
</td>
<td align="right">
140
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
22
</td>
<td align="right">
22
</td>
<td>
</td>
<td align="right">
52.32
</td>
<td align="right">
26
</td>
<td align="right">
810
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
23
</td>
<td align="right">
23
</td>
<td>
</td>
<td align="right">
18.75
</td>
<td align="right">
9
</td>
<td align="right">
450
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
24
</td>
<td align="right">
24
</td>
<td>
</td>
<td align="right">
19.83
</td>
<td align="right">
8
</td>
<td align="right">
635
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
25
</td>
<td align="right">
25
</td>
<td>
</td>
<td align="right">
10.75
</td>
<td align="right">
4
</td>
<td align="right">
150
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
26
</td>
<td align="right">
26
</td>
<td>
San Diego
</td>
<td align="right">
51.00
</td>
<td align="right">
22
</td>
<td align="right">
905
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
27
</td>
<td align="right">
27
</td>
<td>
San Diego
</td>
<td align="right">
16.80
</td>
<td align="right">
7
</td>
<td align="right">
520
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
28
</td>
<td align="right">
28
</td>
<td>
Boston
</td>
<td align="right">
26.16
</td>
<td align="right">
15
</td>
<td align="right">
290
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
29
</td>
<td align="right">
29
</td>
<td>
Boston
</td>
<td align="right">
19.90
</td>
<td align="right">
5
</td>
<td align="right">
500
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
30
</td>
<td align="right">
30
</td>
<td>
Boston
</td>
<td align="right">
24.00
</td>
<td align="right">
6
</td>
<td align="right">
1000
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
31
</td>
<td align="right">
31
</td>
<td>
Boston
</td>
<td align="right">
18.55
</td>
<td align="right">
6
</td>
<td align="right">
225
</td>
<td>
E
</td>
</tr>
<tr>
<td align="right">
32
</td>
<td align="right">
32
</td>
<td>
Boston
</td>
<td align="right">
31.93
</td>
<td align="right">
10
</td>
<td align="right">
775
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
33
</td>
<td align="right">
33
</td>
<td>
Boston
</td>
<td align="right">
16.95
</td>
<td align="right">
4
</td>
<td align="right">
212
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
34
</td>
<td align="right">
34
</td>
<td>
Austin
</td>
<td align="right">
7.00
</td>
<td align="right">
1
</td>
<td align="right">
144
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
35
</td>
<td align="right">
35
</td>
<td>
Austin
</td>
<td align="right">
14.00
</td>
<td align="right">
3
</td>
<td align="right">
126
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
36
</td>
<td align="right">
36
</td>
<td>
Austin
</td>
<td align="right">
37.03
</td>
<td align="right">
12
</td>
<td align="right">
655
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
37
</td>
<td align="right">
37
</td>
<td>
Louisville
</td>
<td align="right">
18.62
</td>
<td align="right">
10
</td>
<td align="right">
420
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
38
</td>
<td align="right">
38
</td>
<td>
Louisville
</td>
<td align="right">
16.10
</td>
<td align="right">
7
</td>
<td align="right">
150
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
39
</td>
<td align="right">
39
</td>
<td>
Louisville
</td>
<td align="right">
24.38
</td>
<td align="right">
8
</td>
<td align="right">
360
</td>
<td>
P
</td>
</tr>
<tr>
<td align="right">
40
</td>
<td align="right">
40
</td>
<td>
Louisville
</td>
<td align="right">
64.75
</td>
<td align="right">
32
</td>
<td align="right">
1530
</td>
<td>
P
</td>
</tr>
</table>

``` r
# Split data into Estimation and Prediction sets
# Distinguish between original data and new data

dfP <- subset(Lex16_3, EorP != "E")
dfE <- subset(Lex16_3, EorP != "P")


# list datafiles "P" and "E"
dfP
```

Obs City time cases distance EorP 1 1 <NA> 16.68 7 560 P 2 2 <NA> 11.50
3 220 P 3 3 <NA> 12.03 3 340 P 11 11 <NA> 40.33 16 688 P 12 12 <NA>
21.00 10 215 P 14 14 <NA> 19.75 6 462 P 16 16 <NA> 29.00 10 776 P 17 17
<NA> 15.35 6 200 P 19 19 <NA> 9.50 3 36 P 26 26 San Diego 51.00 22 905 P
28 28 Boston 26.16 15 290 P 32 32 Boston 31.93 10 775 P 33 33 Boston
16.95 4 212 P 34 34 Austin 7.00 1 144 P 35 35 Austin 14.00 3 126 P 36 36
Austin 37.03 12 655 P 37 37 Louisville 18.62 10 420 P 38 38 Louisville
16.10 7 150 P 39 39 Louisville 24.38 8 360 P 40 40 Louisville 64.75 32
1530 P

``` r
dfE
```

Obs City time cases distance EorP 4 4 <NA> 14.88 4 80 E 5 5 <NA> 13.75 6
150 E 6 6 <NA> 18.11 7 330 E 7 7 <NA> 8.00 2 110 E 8 8 <NA> 17.83 7 210
E 9 9 <NA> 79.24 30 1460 E 10 10 <NA> 21.50 5 605 E 13 13 <NA> 13.50 4
255 E 15 15 <NA> 24.00 9 448 E 18 18 <NA> 19.00 7 132 E 20 20 <NA> 35.10
17 770 E 21 21 <NA> 17.90 10 140 E 22 22 <NA> 52.32 26 810 E 23 23 <NA>
18.75 9 450 E 24 24 <NA> 19.83 8 635 E 25 25 <NA> 10.75 4 150 E 27 27
San Diego 16.80 7 520 E 29 29 Boston 19.90 5 500 E 30 30 Boston 24.00 6
1000 E 31 31 Boston 18.55 6 225 E

``` r
# Create model using estimation set and compare to model using full set. Compare to Table 11.5 on p. 384

# model using estimation data
model.dfE <- lm(dfE$time ~ dfE$cases + dfE$distance)

# analysis using estimation data
xtable(summary(model.dfE))
```

% latex table generated in R 4.0.2 by xtable 1.8-4 package % Wed Mar 17
19:19:49 2021
``` r
xtable(anova(model.dfE))
```

% latex table generated in R 4.0.2 by xtable 1.8-4 package % Wed Mar 17
19:19:49 2021
``` r
# model using all data
model.Lex16_3 <- lm(Lex16_3$time ~ Lex16_3$cases + Lex16_3$distance)

# analysis using all data
xtable(summary(model.Lex16_3))
```

% latex table generated in R 4.0.2 by xtable 1.8-4 package % Wed Mar 17
19:19:49 2021
``` r
xtable(anova(model.Lex16_3))
```

% latex table generated in R 4.0.2 by xtable 1.8-4 package % Wed Mar 17
19:19:49 2021
``` r
# Reproduce Table 11.6 on p. 385

# y predicted -- using predicted values in model created from the estimated values
y_hat <- model.dfE$coefficients[1] +
  model.dfE$coefficients[2]*dfP$cases +
  model.dfE$coefficients[3]*dfP$distance 

# predict error
predict_error <- dfP$time - y_hat

table_11pt3 <- data.frame(dfP$Obs,
                          dfP$time,
                          y_hat,
                          predict_error)
                                 
out <- table_11pt3
colnames(out) <- c("Observation, $i$",
                   "Observed, $y_i$",
                   "LSF Predicted, $\\hat{y}_i$",
                   "LSF Prediction Error, $e_i = y_i-\\hat{y}_i$")

tab <- (xtable(out,digits=c(0,0,2,4,4)))
print(tab, type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Mar 17 19:19:49 2021 -->
<table border="1">
<tr>
<th>
</th>
<th>
Observation, *i*
</th>
<th>
Observed, *y*<sub>*i*</sub>
</th>
<th>
LSF Predicted, *ŷ*<sub>*i*</sub>
</th>
<th>
LSF Prediction Error,
*e*<sub>*i*</sub> = *y*<sub>*i*</sub> − *ŷ*<sub>*i*</sub>
</th>
</tr>
<tr>
<td align="right">
1
</td>
<td align="right">
1
</td>
<td align="right">
16.68
</td>
<td align="right">
21.4976
</td>
<td align="right">
-4.8176
</td>
</tr>
<tr>
<td align="right">
2
</td>
<td align="right">
2
</td>
<td align="right">
11.50
</td>
<td align="right">
10.3199
</td>
<td align="right">
1.1801
</td>
</tr>
<tr>
<td align="right">
3
</td>
<td align="right">
3
</td>
<td align="right">
12.03
</td>
<td align="right">
11.9508
</td>
<td align="right">
0.0792
</td>
</tr>
<tr>
<td align="right">
4
</td>
<td align="right">
11
</td>
<td align="right">
40.33
</td>
<td align="right">
37.9901
</td>
<td align="right">
2.3399
</td>
</tr>
<tr>
<td align="right">
5
</td>
<td align="right">
12
</td>
<td align="right">
21.00
</td>
<td align="right">
21.7264
</td>
<td align="right">
-0.7264
</td>
</tr>
<tr>
<td align="right">
6
</td>
<td align="right">
14
</td>
<td align="right">
19.75
</td>
<td align="right">
18.5265
</td>
<td align="right">
1.2235
</td>
</tr>
<tr>
<td align="right">
7
</td>
<td align="right">
16
</td>
<td align="right">
29.00
</td>
<td align="right">
29.3509
</td>
<td align="right">
-0.3509
</td>
</tr>
<tr>
<td align="right">
8
</td>
<td align="right">
17
</td>
<td align="right">
15.35
</td>
<td align="right">
14.9657
</td>
<td align="right">
0.3843
</td>
</tr>
<tr>
<td align="right">
9
</td>
<td align="right">
19
</td>
<td align="right">
9.50
</td>
<td align="right">
7.8192
</td>
<td align="right">
1.6808
</td>
</tr>
<tr>
<td align="right">
10
</td>
<td align="right">
26
</td>
<td align="right">
51.00
</td>
<td align="right">
50.7745
</td>
<td align="right">
0.2255
</td>
</tr>
<tr>
<td align="right">
11
</td>
<td align="right">
28
</td>
<td align="right">
26.16
</td>
<td align="right">
30.9417
</td>
<td align="right">
-4.7817
</td>
</tr>
<tr>
<td align="right">
12
</td>
<td align="right">
32
</td>
<td align="right">
31.93
</td>
<td align="right">
29.3373
</td>
<td align="right">
2.5927
</td>
</tr>
<tr>
<td align="right">
13
</td>
<td align="right">
33
</td>
<td align="right">
16.95
</td>
<td align="right">
11.8504
</td>
<td align="right">
5.0996
</td>
</tr>
<tr>
<td align="right">
14
</td>
<td align="right">
34
</td>
<td align="right">
7.00
</td>
<td align="right">
6.0086
</td>
<td align="right">
0.9914
</td>
</tr>
<tr>
<td align="right">
15
</td>
<td align="right">
35
</td>
<td align="right">
14.00
</td>
<td align="right">
9.0424
</td>
<td align="right">
4.9576
</td>
</tr>
<tr>
<td align="right">
16
</td>
<td align="right">
36
</td>
<td align="right">
37.03
</td>
<td align="right">
30.9848
</td>
<td align="right">
6.0452
</td>
</tr>
<tr>
<td align="right">
17
</td>
<td align="right">
37
</td>
<td align="right">
18.62
</td>
<td align="right">
24.5125
</td>
<td align="right">
-5.8925
</td>
</tr>
<tr>
<td align="right">
18
</td>
<td align="right">
38
</td>
<td align="right">
16.10
</td>
<td align="right">
15.9254
</td>
<td align="right">
0.1746
</td>
</tr>
<tr>
<td align="right">
19
</td>
<td align="right">
39
</td>
<td align="right">
24.38
</td>
<td align="right">
20.4187
</td>
<td align="right">
3.9613
</td>
</tr>
<tr>
<td align="right">
20
</td>
<td align="right">
40
</td>
<td align="right">
64.75
</td>
<td align="right">
75.6609
</td>
<td align="right">
-10.9109
</td>
</tr>
</table>
