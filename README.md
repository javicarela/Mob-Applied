# Mob-Applied
In recent years, biased and unbiased recursive partitioning algorithms have been applied to datasets from very different scopes, within fields of knowledge such as medicine, engineering or finance. In this work we will try to apply unbiased recursive partitioning to a set European companies listed on the stock exchange.

In summary, Model-based recursive partitioning (MOB) is fitted following the next basic steps:
- 1. Through the minimization of the objective function we obtain the estimated coefficients at the current node for all observations.
- 2. Evaluate the stability of the estimated parameter with respect to all  partition variables. Yesthere is some instability select the variable associated with the greatest instability parameters, otherwise stop.
- 3. Calculate the locally minimizing  the objective function associated with each node split point , either for a fixed number oradaptive splits.
- 4. Divide the node into child nodes and repeat the process.


In this repository, the advantages of model-based recursive partitioning (MOB) have been verified. It has also been observed how the estimation varies when the nature of the response variable changes, that is, by having a continuous, binary, or ordinal response variable. It has also been tested how MOB can fit different models in the terminal nodes such as Logit, Lineal model, or Random Forest.
