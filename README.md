# Soil Quality Index (SQI) Shiny Application

Interactive Shiny dashboard for calculating and mapping a Soil Quality Index (SQI) using PCA-based weights and basic geostatistics.

## Features

- **Step-by-step dashboard** covering data upload, exploratory analysis, PCA, SQI calculation, spatial interpolation, and results review
- **Flexible normalization** with “more is better”, “less is better”, and “optimal range” options plus default ranges for common variables
- **PCA-driven weights** built with FactoMineR and factoextra tooling
- **Spatial interpolation** via ordinary kriging (automated variogram fitting) or inverse distance weighting
- **Outputs** including SQI tables, classification summaries, histograms, and downloadable CSV/GeoTIFF files

## Project Structure

- `app.R`: Main Shiny application containing UI and server logic
- `requirements.R`: Helper script that installs required packages and sources `app.R`

## Installation and Setup

### Prerequisites

- R (version 4.0 or higher)
- RStudio or another R environment (recommended)

### Quick Start

1. Clone or download this repository.
2. Open an R session in the project directory.
3. Run the launcher script:

   ```r
   source("requirements.R")
   ```

   The script installs missing packages (from CRAN) and starts the Shiny app.

### Manual Installation

If you prefer installing dependencies yourself:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "leaflet",
  "readxl", "writexl", "tidyverse", "FactoMineR", "factoextra",
  "corrplot", "sf", "terra", "gstat", "automap", "sp", "caret"
))

shiny::runApp("app.R")
```

## Application Workflow

### 1. Guide

- Comprehensive instructions and recommendations
- Data requirements and format specifications
- Normalization type explanations
- Recommended optimal ranges for common soil variables

### 2. Input Data

- Upload CSV or Excel files containing soil property data
- **Required columns**: X, Y (spatial coordinates)
- **Soil properties**: pH, organic matter, texture, nutrients, etc.
- Coordinate system selection (UTM, Geographic)
- Optional study area boundary upload (GeoJSON, GeoPackage)

### 3. Exploratory Data Analysis

- Variable selection interface
- Correlation matrix visualization
- Automatic removal of highly correlated variables (threshold customizable)
- Summary statistics for all variables

### 4. PCA Analysis

- Principal Component Analysis with scaling options
- Scree plot and biplot visualization
- Variable contributions to principal components
- Automatic weight calculation based on significant PCs (eigenvalue > 1)

### 5. SQI Calculation

- **Three normalization types**:
  - **More is Better**: Higher values = better quality (e.g., organic matter, nutrients)
  - **Less is Better**: Lower values = better quality (e.g., bulk density)
  - **Optimal Range**: Values within specific range = optimal quality (e.g., pH, texture)
- Customizable optimal ranges for each variable
- Real-time SQI calculation with statistical summaries
- Distribution visualization

### 6. Spatial Assessment

- **Interpolation methods**:
  - **Ordinary Kriging**: Automatic variogram fitting using automap
  - **Inverse Distance Weighting**: Customizable power and neighborhood size
- Grid size selection for interpolation resolution
- Variogram visualization for kriging when automatic fitting succeeds

### 7. Results

- Interactive Leaflet map showing spatial SQI distribution
- SQI classification into quality classes (Very Low to Very High)
- Final text summary of key statistics
- **Download options**:
  - SQI data as CSV
  - SQI raster as GeoTIFF

## Data Requirements

### Input Data Format

Your CSV or Excel file must contain:

1. **Coordinate columns**:

   - `X`: X-coordinate (Easting)
   - `Y`: Y-coordinate (Northing)
2. **Soil property columns** (examples):

   - `pH`: Soil pH
   - `OM`: Organic matter (%)
   - `Clay`, `Silt`, `Sand`: Texture components (%)
   - `CEC`: Cation Exchange Capacity
   - `BD`: Bulk density
   - `EC`: Electrical conductivity
   - `P`, `K`: Phosphorus and Potassium content
   - Any other numerical soil properties

### Example Data Structure

```
X        Y        pH    OM    Clay  Silt  Sand  CEC   BD    EC    P     K
345678   8123456  6.8   2.1   18    35    47    15.2  1.35  3.2   22    450
345789   8123567  7.2   1.8   22    38    40    18.5  1.28  2.8   28    520
...
```

**Note**: The application automatically detects all numerical variables (excluding X,Y coordinates) for analysis. You can select which variables to include in your SQI calculation.

### Coordinate Reference Systems

The application supports common coordinate systems:

- **UTM Zone 18S (EPSG:32718)** - Default for Peru
- **WGS84 Geographic (EPSG:4326)** - Latitude/Longitude
- **Custom EPSG codes** - Enter any valid EPSG code

### Study Area Boundaries (Optional)

Upload study area boundaries in:

- **GeoJSON** format (.geojson)
- **GeoPackage** format (.gpkg)

## Normalization Guidelines

### Recommended Normalization Types

| Variable       | Type           | Optimal Range | Rationale                                         |
| -------------- | -------------- | ------------- | ------------------------------------------------- |
| pH             | Optimal Range  | 6.5 - 7.5     | Most crops prefer slightly acidic to neutral pH   |
| Organic Matter | More is Better | -             | Higher OM improves soil structure and fertility   |
| Clay (%)       | Optimal Range  | 10 - 25       | Balanced texture for water retention and drainage |
| Silt (%)       | Optimal Range  | 30 - 50       | Good water holding capacity                       |
| Sand (%)       | Optimal Range  | 40 - 60       | Adequate drainage and aeration                    |
| Bulk Density   | Less is Better | -             | Lower BD indicates less compaction                |
| CEC            | More is Better | -             | Higher CEC means better nutrient retention        |
| EC (dS/m)      | Optimal Range  | 2.0 - 15.0    | Moderate salinity range                           |
| Phosphorus     | More is Better | -             | Higher P availability for plants                  |
| Potassium      | More is Better | -             | Higher K availability for plants                  |

### Custom Optimal Ranges

You can define custom optimal ranges based on:

- **Local soil conditions**
- **Specific crop requirements**
- **Regional standards**
- **Expert knowledge**

## Troubleshooting & Known Issues

- **Coordinate columns**: The app requires numeric `X` and `Y` columns; they are not coerced from character data.
- **Custom CRS**: When choosing the “Custom” CRS option you must supply a valid EPSG code; leaving it blank will raise an error when spatial objects are created.
- **Shapefile uploads**: The boundary uploader only accepts a single file, so multi-file Shapefiles are not supported—use GeoJSON or GeoPackage instead.
- **Manual variogram workflow**: Ordinary kriging currently relies on automatic variogram fitting; disabling it leaves no variogram model available and the interpolation step fails.
- **Spatial coverage**: Sparse or clustered sample points can lead to poor interpolation results; consider verifying spatial coverage before running kriging/IDW.

## Technical Details

### Statistical Methods

- **PCA**: Principal Component Analysis using FactoMineR package
- **Kriging**: Ordinary kriging with automatic variogram fitting (automap)
- **IDW**: Inverse Distance Weighting with customizable parameters
- **Correlation analysis**: Pearson correlation with configurable thresholds

### Spatial Analysis

- **Coordinate systems**: Support for projected and geographic coordinates
- **Interpolation**: Regular grid-based prediction
- **Boundary clipping**: Optional study area constraint
- **Visualization**: Interactive Leaflet maps with customizable symbology

## Citation

If you use this application in your research, please cite:

```
[Your Citation Information Here]
Soil Quality Index Calculator v1.0
[Your Institution/Contact Information]
```

## Support

- Review the built-in “Guide” tab for workflow tips.
- Check the troubleshooting notes above for common pitfalls.
