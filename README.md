# ColourInteractionExperiment

## Processing data


### Extract the data from the ELAN files

The python scripts use the Pympi library.

```
cd processing
python3 ./eaf2csv_colours.py
```

Creates `data/processedData/variants.csv`

### Clean and process the data

```
cd processing
R -f cleanResultsFiles.R
```

Creates `data/processedData/variants_summary.csv` and ``data/processedData/variants_processed.csv`.

This script uses the file `processing/signChanges.R` to convert some of the variant labels.

## Running the statistical analysis

The main statistical analysis is an R markdown script in `analysis/lmerModel2.Rmd`.  This produces the pdf `analysis/lmerModel2.pdf`.

