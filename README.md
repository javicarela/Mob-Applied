# Mob-Applied

In recent years, biased and unbiased recursive partitioning algorithms have been applied to datasets from very different scopes, within fields of knowledge such as medicine, engineering or finance. In this work we will try to apply unbiased recursive partitioning to a set European companies listed on the stock exchange.
# Algorithm

In summary, Model-based recursive partitioning **(MOB)** is fitted following the next basic steps:
- 1. Through the minimization of the objective function we obtain the estimated coefficients at the current node for all observations.
- 2. Evaluate the stability of the estimated parameter with respect to all  partition variables. Yesthere is some instability select the variable associated with the greatest instability parameters, otherwise stop.
- 3. Calculate the locally minimizing  the objective function associated with each node split point , either for a fixed number oradaptive splits.
- 4. Divide the node into child nodes and repeat the process. 


# Data

We will use data extracted from the **Amadeus** database. We will consider data from European companies for 2017, ordered by total assets. The data of each company will refer, on the one hand, to the particular accounting indicators of each company, such as leverage or operating margin, and on the other hand macroeconomic data of the country in which it is listed, such as the percentage growth of GDP or the unemployment rate. 

- **1.Choosing a database** that would provide us with a reliable data set to the even complete. The Amadeus database contains complete information about 21 million companies in Europe.

- **2.Election of the set:** Although the population will be European listed companies, theThe sample will be those companies that were the largest in Europe in 2017, taking as a measure of size the total asset. From here we select a total of **21,739 observations.**

- **3.First filtering:** We select only those that have a listed price in the years 2016 and 2017, that is, we choose only the largest companies listed companies, thus eliminating branches and unlisted companies. The new set it has **1,539 observations.**

- **4.Choice of variables:** These indicators are divided between fundamental-accounting and macroeconomic. I know also include 2 categorical variables: Country in which it operates (Country) and industry which belongs (Industry).

- **5. Adjustment and calculation of variables:** We calculate some variables that will be of interest to us:
    - Variation rates of the price of each share between 2016 and 2017. This will be the variableto explain.
    - Standard deviation of each share, calculated from the monthly price of each company during the year 2017. It will serve as a measure of volatility.
    - Name change of some variables (Total assets th EUR 2017-> AsstT, CashFlow th EUR 2017 -> CashF,… etc).
 
 
In this repository, the advantages of model-based recursive partitioning (MOB) have been verified. It has also been observed how the estimation varies when the nature of the response variable changes, that is, by having a continuous, binary, or ordinal response variable. It has also been tested how MOB can fit different models in the terminal nodes such as Logit, Lineal model, or Random Forest.

# References

[1]     **Zeileis A, Hothorn T, Hornik K (2008).** *Model-Based Recursive Partitioning. Journal of
Computational and Graphical Statistics*, 17(2), 492–514.

[2]     **AMADEUS** *A database with comparable financial information on companies
public and private in Europe(2010)*. Bureau van Dijk Electronic Publishing


