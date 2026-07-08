# Whale Migration Sightings Map

This project is an interactive geospatial visualization built from real-world whale occurrence data, created as part of my personal portfolio to demonstrate API integration, data cleaning, and geospatial visualization skills in Python.

The dataset is pulled live from two international biodiversity databases (GBIF and OBIS), covering global sightings across 8 whale species, including:

- Blue Whale
- Fin Whale
- Orca
- Sperm Whale
- Humpback Whale
- Gray Whale
- Narwhal
- Beluga Whale

**Key tasks included:**

- Fetching and caching live data from external APIs
- Cleaning, deduplicating, and combining multi-source records
- Applying coordinate rounding for public data privacy
- Creating an interactive map with `folium`:
  - Per-species toggleable layers
  - Sightings density heatmap
  - Custom legend and map caption

The project showcases my ability to combine external data sources into a clear, interactive tool, connecting my background in marine conservation with applied data analytics.

**Tools:** `pandas`, `requests`, `folium`, `numpy`
