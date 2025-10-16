# Soil Quality Index (SQI) App – User Guide

## 1. Launch the Application

1. Open an R session in the project directory.
2. Run `source("requirements.R")` to install dependencies and start the Shiny app.
3. Wait for the browser window to open; keep the R session running while you work in the app.

## 2. Understand the Workflow (Guide Tab)

- Read the overview describing each workflow stage.
- Confirm the data requirements (numeric soil variables plus `X` and `Y` coordinates).
- Note the recommended normalization types and default optimal ranges.

## 3. Upload and Inspect Data (1. Input Data)

1. Use **Data Upload** to load a CSV or Excel file.
   - Ensure the file contains columns named `X` and `Y` with coordinates.
   - Choose the correct separator and quote options for CSV files.
2. In **Coordinate System**, pick an EPSG code that matches your coordinates.
   - Select **Custom** only if you can provide a valid EPSG code (for example `EPSG:32718`).
3. Optionally upload a boundary file in GeoJSON or GeoPackage format.
4. Review **Data Preview** to validate column names and units.
5. Check **Data Summary** to confirm value ranges and missing data counts.

## 4. Explore Variables (2. Exploratory Data)

1. In **Variable Selection**, choose the numeric variables to include in the analysis (X and Y are excluded automatically).
2. Set the **Correlation Threshold** (default 0.98) and click **Remove Highly Correlated Variables**.
   - Removed variables will be listed in the **Removed Variables** panel.
3. Inspect the **Correlation Matrix** plot to spot multi-collinearity issues.
4. Review **Variable Statistics** for means, standard deviations, min/max, and missing counts.

## 5. Run PCA (3. PCA Analysis)

1. Click **Run PCA Analysis** after confirming the cleaned variable list.
2. Adjust options as needed:
   - **Scale Variables** (on by default) standardizes the data.
   - **Eigenvalue Threshold** controls how many principal components contribute to weights (default 1).
3. Examine outputs:
   - **Scree Plot** indicates the number of significant components.
   - **Variable Contributions** shows each variable’s contribution per component.
   - **PCA Weights** lists the normalized weights used later in SQI.
   - **PCA Biplot** visualizes relationships among variables and components.

## 6. Configure Normalization and Compute SQI (4. SQI Calculation)

1. For each variable box under **Normalization Settings**:
   - Choose a normalization type: *More is Better*, *Less is Better*, or *Optimal Range*.
   - If using *Optimal Range*, supply minimum and maximum optimal values.
2. Press **Calculate SQI**.
3. Review the results:
   - **SQI Statistics** prints summary statistics (min, max, mean, quartiles).
   - **SQI Distribution** histogram shows the spread of index values.
   - **SQI by Sample Points** bar chart compares individual samples with the mean.

## 7. Perform Spatial Assessment (5. Spatial Assessment)

1. Confirm that your dataset includes valid `X` and `Y` coordinates before proceeding.
2. Choose **Interpolation Method**:
   - **Ordinary Kriging** (default) requires successful automatic variogram fitting.
   - **Inverse Distance Weighting (IDW)** allows manual tuning of power and neighbor count.
3. Set **Grid Cell Size** in meters to define interpolation resolution.
4. Click **Run Interpolation**.
5. Review the outputs:
   - **Variogram** plot (kriging only) confirms the fitted model.
   - **Interpolation Summary** reports grid details and predicted SQI range.
   - Successful runs store both point predictions and a raster surface.

## 8. Review and Export Results (6. Results)

1. Use the **Spatial Distribution Map** to explore interpolated SQI values.
   - Raster and point layers can be toggled via the layer control (when both are available).
2. Check **SQI Classification** for counts and percentages per quality class.
3. Read the **Final Results Summary** for a concise textual recap.
4. Download outputs as needed:
   - **Download SQI Data (CSV)** exports cleaned samples with SQI scores.
   - **Download SQI Raster (GeoTIFF)** saves the interpolated raster (if generated successfully).

## 9. Troubleshooting Tips

- Missing or non-numeric `X`/`Y` columns prevent spatial features from working; correct column types in the input file.
- When selecting **Custom** CRS, the EPSG code must be supplied before running interpolation.
- Upload boundaries as GeoJSON or GeoPackage; multi-file Shapefiles are not supported.
- If kriging fails, retry with automatic variogram fitting enabled or switch to IDW.
- Sparse sampling may cause interpolation artifacts; verify coverage or adjust grid size.
