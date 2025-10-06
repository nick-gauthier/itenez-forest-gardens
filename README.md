# Pre-Columbian Forest Gardens in Bolivia's Iténez Preserve

**Mapping 180 km² of ancient landscape modification using AI-assisted archaeological detection**

## Overview

This repository contains the analysis code and documentation for identifying extensive pre-Columbian forest gardens and anthropogenic soils across Bolivia's 4,000 km² Iténez Forest Preserve. Using satellite imagery and AI-assisted detection, we mapped over 350 individual sites spanning 180 km² of modified landscapes—filling a major gap in Amazonian archaeological knowledge.

## Key Findings

-   **180 km²** of anthropogenic forest gardens identified (5% of preserve area)
-   **350+ individual sites** ranging from 5-700 hectares
-   Hierarchical settlement patterns with extensive earthwork networks
-   Continuous landscape modification between previously studied regions
-   Living heritage: local communities still harvest cacao from these ancient forests

## Methods

-   **Satellite Analysis**: Sentinel-2 imagery from driest periods (2023-2024)
-   **AI Detection**: GPT-4.1 and Moondream2 vision models for pattern recognition
-   **Human Validation**: Expert archaeological interpretation and manual mapping

## Repository Structure

```         
├── index.qmd             # main analysis notebook 
├── R/                    # Analysis scripts
│   └── moondream.R      # AI detection functions
├── data/
│   ├── derived/         # Processed datasets (downloaded separately)
│   └── raw/             # Original satellite imagery
├── images/              # Figures and maps
└── README.md
```

## Requirements

-   R 4.0+
-   Google Earth Engine access
-   Required packages: `tidyverse`, `sf`, `mapview`, `tidyterra` `ellmer`

## Usage

1.  Clone repository
2.  Install required R packages
3.  Run main analysis notebook
4.  View results in interactive maps

## Significance

This work demonstrates that the "pristine" Amazon was extensively managed by Indigenous peoples, with ecological signatures persisting today. The human-AI partnership approach offers a scalable model for archaeological survey across forested regions.

## Citation

Gauthier, N. (2025). Pre-Columbian forest gardens span 180 km² in Bolivia's Iténez preserve.

## Contact

Nicolas Gauthier - Florida Museum of Natural History, University of Florida

------------------------------------------------------------------------
