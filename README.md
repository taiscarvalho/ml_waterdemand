# Machine learning techniques for water demand modeling

The codes contained here implement the methods of the paper "Urban water demand modeling using machine learning techniques: Fortaleza – Brazil case study", accepted for publication in the Journal of Water Resources Planning and Management. The code includes the R implementation of the Iterative Input Selection (IIS) algorithm proposed by Galelli and Castelletti (2013). The original Matlab implementation of the IIS algorithm (Galelli and Castelletti; 2013) can be found [here](https://github.com/stefano-galelli/MATLAB_IterativeInputSelection).

Galelli, S., and A. Castelletti (2013), Tree-based iterative input variable selection for hydrological modeling, Water Resour. Res., 49(7), 4295-4310.

## Requirements
Before running the codes, you need to install the following packages:
```
install.packages(randomForest)
install.packages(RSNNS)
install.packages(kohonen)
install.packages(Metrics)
install.packages(dplyr)
```

## Description
The codes and data contained here are as follows:
* `performance_measures`: compute the desired performance measure
* `variable_ranking_rf`: rank the variables according to the IncMSE and the RF algorithm
* `som_clustering`: performs SOM based clustering
* `miso_ann`: runs ANN with multiple inputs/single output
* `siso_ann`: runs ANN with single input/single output
* `iterative_input_selection`: runs the IIS algorithm with RF and ANN

## License
These codes are free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published bythe Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see https://www.gnu.org/licenses/.
