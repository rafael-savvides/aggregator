# aggregator

Functions for reading various personal data.

Supports data caching.

To cache datasets:

- `init_config()` creates a config file.
- `edit_config()` opens the config file to add paths to raw data.
- `update_cache()` updates cached datasets listed in the config.

To load cached datasets: 

- `load_data(name)` loads the cached dataset `name` 
- `available_data()` shows all cached datasets
