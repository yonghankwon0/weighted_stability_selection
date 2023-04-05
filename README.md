# weighted stability selection

## Summary
Stability selection (Meinshausen and Bühlmann, 2010) is a variable selection algorithm based on resampling a dataset. Based on stability selection, we propose weighted stability selection to select variables by weighing them using the area under the receiver operating characteristic curve (AUC) from additional modelling. The proposed method was evaluated using an extensive simulation study to measure the true positive rate (TPR), positive predictive value (PPV), and stability of variable selection. We also assessed the predictive ability of the method using a validation set. The proposed method performed similarly to stability selection in terms of the TPR, PPV, and stability. The AUC of the model fitted on the validation set with the selected variables of the proposed method was consistently higher in specific scenarios. Moreover, when applied to a radiomics dataset, the proposed method had a higher AUC when fewer variables were selected. A major advantage of the proposed method is that it enables researchers to select variables intuitively using relatively simple parameter settings.

![flow_chart](https://user-images.githubusercontent.com/31601961/230020510-29148ba3-6b10-493a-8b33-01a345e3a290.jpg)

https://www.nature.com/articles/s41598-023-32517-4

## List of Files
1. data_generate.R = R file for simulation data generation
2. weighted_stabiltiy_selection.R = R file for suggested "weighted_stability_selection"  
  

## Code example
```{r, include = FALSE}
data_generated <- data_generate(n = 500, p = 1000, p_signal = 20, coef_1 = 2, data_cov = "toep", prevalence = 0.5)
data <- data_generated$data[,-1]
y <- colnames(data)[dim(data)[2]]
doParallel::registerDoParallel(detectCores())
weighted_stabiltiy_selection(data, y, B = 50)
```
