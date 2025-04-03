# Proposal for Semester Project

<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github. Please do not Rename this file (Readme.md has a special meaning on GitHub).

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement Analysis Geo 880**

| Semester:      | FS25                                     |
|:---------------|:---------------------------------------- |
| **Data:** | Positional data from 115 tagged individuals |
| **Title:** | Habitat use and movement patterns of the spotted Nutcracker (Nucifraga caryocatactes) |
| **Student 1:** | Robin Merz |
| **Student 2:** | Claude Widmer |
|**Github:**|[Github Repository](https://github.com/widmerc/GEO880_Project)|

![Tannenhäher (Vogelwarte.ch)](./data/Tannenhäher.jpg)


## Abstract

<!-- (50-60 words) -->

Even though the spotted Nutcracker (Nucifraga caryocatactes) is locally common in many areas across the Eurasian continent, little is known about many aspects of the species (Billerman et al. 2020).
During this project we will analyze GPS and sensor data from 115 tagged individuals to gain insights on their habitat use and movements across various life cycle phases.
The aim of the study will be to identify potential seasonal differences in habitat use as well as the identification of important areas within individual home-ranges (harvesting-, caching-, nesting sites). -> Annahme, einfach gut differenzieren.
In a last step, the similarities between individuals, as well as potential overlaps of home-ranges will be determined.
The results will be presented in an interactive RShiny application, allowing users to explore movements and statistics. -> Wenn nicht mit shiny, dann ist es auch hilfreich.


## Research Questions
<!-- (50-60 words) -->
-   How do the movement patterns of birds differ during various life cycle phases? (gut)
-   What differences exist between adult and juvenile movement patterns?
-   Can machine learning help us to identify important areas within individual home-ranges?
-   To what extent do individual home ranges overlap seasonally?
-   To what extent are harvesting-, caching- and nesting-sites overlapping among individuals?

## Results / products
<!-- (50-100 words) -->
<!-- What do you expect, anticipate? -->
We expect to identify specific foraging and nesting clusters through clustering algorithms.
Additionally, we expect to find significant differences in movements across various life cycle phases.
Given that little is known about the birds' habitat use, this analysis will provide new preliminary insights into their behavioral patterns.

The final product includes an interactive RShiny application with a Leaflet map, showcasing bird movements and clustering results.
To make our results available for stakeholders and promote further insight into this field, a story-map web application will be developed to visually present our objectives, research questions, and findings in an engaging, interactive way, effectively displaying the birds and their data.

## Data
<!-- (100-150 words) -->
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->
Our dataset consists of raw GPS and sensor data from 115 birds, with a subset remaining active as of March 2025.
Currently, the dataset includes approximately 50,000 GPS data points (time, longitude, latitude, altitude), along with additional key information such as the bird's ID and physical measurements (weight, wing length, bill length, and age at capture).
The data is complemented by metadata such as tag type (VHF, Interex, 5G) and capture date and has kindly been provided by the Swiss Ornithological Institute (CH) and the Senckenberg Institute (DE).
While some additional data will be collected through ongoing fieldwork, the dataset is largely complete, stored in a well-structured, non-relational CSV file, and ready for analysis.
However, the data will require substantial cleaning due to gaps caused by signal loss and potential inconsistencies arising from the use of different tag types.

## Analytical concepts
<!-- (100-200 words) -->
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->
We use various methods to study the birds' movement patterns and behaviors, helping us to understand their habitat use and seasonal changes.
Key methods include:

-   **Clustering analysis:** We identify important areas like nesting, caching, and harvesting sites by grouping locations where birds spend a lot of time, using methods like DBSCAN and k-Means.

(-   **Machine learning integration:** We train models to detect movement patterns in GPS data, helping us spot behaviors like seasonal shifts and specific site selections.)

-   **Time-series analysis:** We analyze how the birds' movements change over time, especially during different seasons, to detect behavior changes related to food harvesting and caching, as well as breeding.

-   **Overlap analysis:** We compare the areas used by different birds to see if they share home ranges as well as important areas within them, helping us to understand competition and interactions.

(-   **Similarity analysis of GPS tracks:** We examine the movement paths of birds to group those with similar behaviors.)


## R concepts
<!-- (50-100 words) -->
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->
Main R packages used: 

- **Spatial analysis:** `sf` for handling spatial data, `spatstat` for spatial point pattern analysis and KDE.\
- **Movement analysis:** `amt` for trajectory analysis, `move` for GPS data processing, `ctmm` for continuous-time movement modeling.\
- **Clustering & machine learning:** `dbscan`, `kmeans`, and `hclust` for detecting patterns and grouping data.\
- **Interactive visualization:** `shiny` for web apps, `leaflet` for maps, `ggplot2` for visualizing movement and clustering results.

## Risk analysis
<!-- (100-150 words) -->
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->

The biggest challenge will be data quality, as different tag types were used, and some GPS transmitters may have gaps or irregular signals.
Additionally, some individuals may have transmitted only a few data points, while others have many.
With around 50,000 data points, managing and analyzing such a large dataset can be complex and time-consuming, which is why data simplification may be necessary.
To achieve this, we may reduce the number of points or individuals by focusing on a specific subset.
Other approaches, such as averaging data or limiting the time frame, could also be considered.
If these challenges arise, we will prioritize clear visualizations and summaries to help users explore the data effectively.
This will enable users to better understand movement patterns and make informed interpretations, providing a foundation for new scientific questions.

## Questions?

<!-- (100-150 words) -->

<!-- Which questions would you like to discuss at the coaching session? -->

-   What additional contextual data might be relevant for our analysis? -> Mischgrad Wald / Höhe / Baumartenanteil
-   Is the Rshiny Approach suitable for this Semester Project? Ja.

## Use of AI

-   AI was used to rewrite self-written sentences and small paragraphs.

## References
- Billerman, S. M., Keeney, B. K., Rodewald, P. G., & Schulenberg, T. S. (Eds.). (2022). *Birds of the World*. Cornell Laboratory of Ornithology, Ithaca, NY, USA. [https://birdsoftheworld.org/bow/home](https://birdsoftheworld.org/bow/home)
