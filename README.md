## Extending finite mixtures of latent trait analyzers for bipartite networks for the analysis of COVID-19 data

These codes allow the application of latent variable models in the context of network data, using a fast variational inference approach. The MLTA model developed by Gollini and Murphy (2014) and Gollini (2020) is able to cluster sending nodes in bipartite networks, as well as to capture the latent variability of network connections within each cluster. The MLTA model is extended to account for the influence of nodal attributes in cluster formation. The goal is to identify clusters of COVID-19 patients adopting similar behaviors against pandemic, also understanding how socio-economic and demographic features, as well as perception of the pandemic, influence cluster formation.

#### Files
* `Data.zip` contains data on the COVID-19 pandemic in Italy.
* `Code.R` contains the code for:
  * the construction of the patient-preventive measures network;
  * the analysis of data on COVID-19;
  * the simulation study to test the performance of the model.
* `R objects` contains some objects required to replicate the simulation study.

#### Main References
* Gollini, I., & Murphy, T. B. (2014). Mixture of latent trait analyzers for model-based clustering of categorical data. Statistics and Computing, 24 (4), 569–588.

* Gollini, I. (2020). A mixture model approach for clustering bipartite networks. Challenges in Social Network Research Volume in the Lecture Notes in Social Networks (LNSN - Series of Springer).

* https://github.com/igollini/lvm4net/tree/master/R

<!---
* Failli, D., Marino, M.F., Martella, F. (2022) Extending finite mixtures of latent trait analyzers for bipartite networks. In Balzanella A., Bini M., Cavicchia C. and Verde R. (Eds.) Book of short Paper SIS 2022 (pp. 540-550), Pearson.
-->
