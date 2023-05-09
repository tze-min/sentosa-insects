# Insect Data Analysis

This is a repository of R scripts and datasets used to explore, clean and visualise insect occurrence records, particularly those found in Sentosa, Singapore. It was created as part of a project to understand the potential of using citizen science data to map the distribution of species within the island.

Created for the Ecological Adaptations Lab at Yale-NUS College in 2021.

## SECTION 1. Project Structure

The folders and its contents are structured as follows. 

```
sentosa-insects
│   README.md
│   sentosa-insects.Rproj
│   start.R
│
├───code
│       01-sentosa-insect-inat-exploration.R
│       02-sentosa-insect-inat-cleaning.R
│       03-sentosa-insect-inat-visualisation.R
│       04-sentosa-insect-inat-leaflet.R
│       05-bees-leaflet.R
|	functions.R
│
├───data
│       processed.zip
│       qgis-file.zip
│       raw.zip
│
├───docs
│       20210511-labmeeting.pptx
│
├───images
│   ├───charts
│   │       inaturalist-sentosa-topusers.png
│   │       inaturalist-sentosa-yearly-user-contributions.png
│   │       inaturalist-sentosa-yearly-user-contributions_no-anomaly.png
│   │       orders-families-sentosa-breakdown_black.png
│   │       orders-families-sentosa-breakdown_colored.png
│   │       sentosa-insect-species-top10.png
│   │       sentosa-insect-species-top15.png
│   │
│   └───insects
│           Abisara saturata.jpg
│
└───maps
        order-maps.zip
        species-maps.zip
```

## SECTION 2. How To Use

### Accessing the code and datasets

1. Unzip `raw.zip` and `processed.zip` in the `data` folder, which contains the occurrence data as well as shapefiles of Singapore's maps, some of which I have used for cleaning the observations. 

	- The `qgis-file.zip` contains a QGIS file I had put together to label polygons of Singapore's islands with their names, as I needed them to figure out which insect observations were in Sentosa. No need to open it as the labelled polygons have been saved into `data\raw`.

2. Click on `sentosa-insects.Rproj` to open the project in your RStudio with all its associated files loaded.

3. The R scripts in the `code` folder contains the progression of my data clean up and visualisation, sequentially from file `01` to `05`. If you would like to run them, consider doing so segment by segment and going through the comments in each file. 

	- Each of these R scripts has `source("start.R")` as their first line, where running it loads the packages and file paths needed for the project from `start.R`.
	- `functions.R` are generalised functions to create Leaflet maps by order and species, and can be run independently of the other scripts.

4. Please ensure the working directories are accurate with respect to your device as you're running the code. E.g. `data` isn't nestled within a subfolder `data` after you've unzipped it.

### Using the documentation and output (maps and images)

| Folder | Usage |
| --- | --- |
| `docs` | Any documentation made during the data analysis. Contains slides and a report (TBC). |
| `images\charts` | Plots created and saved from the data visualisation process in the R script starting with `03`. |
| `images\insects` | This is where images of insects downloaded from iNaturalist get saved to, also as part of the R script starting with `03`. It currently contains one sample image, but more can be downloaded automatically by running a function within the `03` file. Refer to the script for more details. |
| `maps` | Interactable HTML maps created from the R scripts `04` and `05`. Contains maps by orders and species. |

## SECTION 3. References

### Dataset and Image Sources

- [DIVA-GIS](https://www.diva-gis.org/gdata) for Singapore's shapefiles
- Richards, Dan; Gaw, Leon Yan-Feng; Yee, Alex T.K. (2019): [A high-resolution map of Singapore’s terrestrial ecosystems. figshare. Dataset.](https://doi.org/10.6084/m9.figshare.8267510.v4) for Singapore's maps
- [iNaturalist](https://www.inaturalist.org/observations) for insect occurrence data. Accessed on 31 May 2021 with the query `quality_grade=research&identifications=any&iconic_taxa%5B%5D=Insecta&place_id=6734`.
- [NCBI Taxonomy Database](https://www.ncbi.nlm.nih.gov/taxonomy) for the taxonomy information of species

### Code References

- [R Graph Gallery](https://www.r-graph-gallery.com/index.html) for charts and their code
