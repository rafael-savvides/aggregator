# aggregator

Reads various personal datasets into a usable format.

Usage: 

```{r}
source("scripts/init.R", chdir=T)

data_dict
```

Details:  
- Paths to raw data are in text files inside `data/paths_to_raw`
- Raw data are cleaned and stored in `data/cleaned`. If the raw data have not been modified since the last run, then the stored data are read.
