# aggregator

R package for reading various personal datasets. 

To install, run `R -e "devtools::install()` in its directory. 

Supports data caching. 

- `init_config()` initializes a config json file for adding paths to datasets and adds its path to an environment variable.
- `update_cache()` updates cached datasets listed in the config.

To load cached datasets: 

- `load_data(name)` loads the cached dataset `name` 
- `available_data()` shows all cached datasets
