
Dataset and source code for the paper:

Rubio-Campillo, X., Montanier,, J.M., Rull, G., Bermúdez Lorenzo, J.M., Moros Díaz, J., Pérez González, J., Remesal Rodríguez, J. (2018) **The ecology of Roman trade. Reconstructing provincial connectivity with similarity measures**, __Journal of Archaeological Science__, 92, pp. 37-47. [doi:10.1016/j.jas.2018.02.010](https://doi.org/10.1016/j.jas.2018.02.010)

Structure is defined as follows:

- 'data' contains the raw dataset used in the analysis: 
    - *stamps.csv* gives the coordinates, type and code for each amphora stamp

- 'src' implements the analysis and also generates the figures used in teh paper:
    - *prepareData.py* transforms the raw dataset into tidy data structures for R analysis
    - *mrppBase.R* is a utility file defining the functions used in MRPP analysis
    - *singleMrpp.R* runs Multi Response Permutation Procedure for the entire dataset
    - *selectiveMrpp.R* runs and plots the rolling MRPP for a given range of MNC (Minimum Number of Codes per Site).
    - *cluster.R* generates cladograms using hierarchical clustering methods. It also creates the Supplementary Information charts.

