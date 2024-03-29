# Quantitative Structure Activity Relationship (QSAR) study predicts small molecule binding to RNA structure
The diversity of RNA structural elements and their documented role in human diseases make RNA an attractive therapeutic target. However, progress in drug discovery and development has been hindered by challenges in the determination of high-resolution RNA structures and a limited understanding of the parameters that drive RNA recognition by small molecules, including a lack of validated quantitative structure-activity relationships (QSAR). Herein, we developed QSAR models that quantitatively predict both thermodynamic and kinetic-based binding parameters of small molecules and the HIV-1 TAR model system. Small molecules bearing diverse scaffolds was screened against the HIV-1 TAR using surface plasmon resonance. Then multiple linear regression (MLR) combined with feature selection was performed to afford robust models that allowed direct interpretation of properties critical for both binding strength and kinetic rate constants. These models were externally validated with new molecules and their accurate performance confirmed via comparison to ensemble tree methods. 
![image](https://user-images.githubusercontent.com/103124833/161992793-f47ef5ff-411a-4c60-9531-d9042f4b4445.png)

QSAR workflow. **A**. Input molecules were searched for “protomers” and then searched on conformations of each protomer. Molecular descriptors were calculated for each conformation and averaged based on Boltzmann distribution. **B**. Small molecules binding HIV-1 TAR were characterized via SPR and parameters including KD, kon and koff were fitted globally. **C**. With representative data splitting and lasso-assisted model searching, the final model was selected based on the performance on the separate test set.
## Requirements
#### Python == 3.9.5
- RDKit
- Numpy
- Pandas
#### RStudio == 1.4.1717
- Prospectr
- Rgl
- Ggplot2
- ggfortify
- Magrittr
- Glmnet
- Tree
- randomForest
- Gbm
- matlib
#### MATLAB == R2020a

## Version note:
In the following newer version (after March 21,2023), we decide to delete part of the codes that are not related to the core part of the QSAR baseline model construction. Deletion of these codes won't affect the construction of the baseline model but people might need them to perform classical model construction as compared to the baseline model.
The codes relating to the following packages are going to be deleted:
R packages:
1. gbm (build gradient boost machine model)
2. ggplot2 (visualization)
3. ggfortify (visualization)
4. rgl (3d visualization)
5. tree (build tree-based model)
6. randomForest (build rf model)
7. matlib (matrix linear algebra)

Python package:
1. rdkit (calculate molecular fingerprints for similarity comparison)

## More Information
For more information, please refer to our paper: <br>
Cai, Z., Zafferani, M., Akande, O., & Hargrove, A. (2022). Quantitative Structure Activity Relationship (QSAR) study predicts small molecule binding to RNA structure.<br>
https://doi.org/10.1021/acs.jmedchem.2c00254
## Acknowledgements
This work was supported by Duke University, U.S. National Institutes of Health (U54 AI150470), the Alfred P. Sloan Foundation, and an award from Duke University School of Medicine Core Facilities for use of the BIA Core. Z.C. was supported in part by a Kathleen Zielik Fellowship from the Duke University Chemistry Department. We acknowledge past and present Hargrove Lab members for their assistance with project conceptualization and manuscript editing. We particularly thank former lab members Dr. Neeraj Patwardhan, Ph.D., Dr. Anita Donlic, Ph.D. and Dr. Aline Umuhire Juru, Ph.D. for donating the synthesized DMA, DPF and DMA molecules used here. We thank Duke graduate student Jiayue Xu from interdisciplinary data science for constructive discussions and suggestions. Surface plasmon resonance analyses were performed in the Duke Human Vaccine Institute's Biomolecular Interaction Analysis Shared Resource Facility (Durham, NC) under the direction of Dr. S. Munir Alam and Dr. Brian E. Watts.
## Questions?
Make a [github issue](https://github.com/hargrove-lab/QSAR/issues/new), or email us: zc83@duke.edu :email:
