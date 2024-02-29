## Trial participation in neurodegenerative diseases: barriers and facilitators. A systematic review and meta-analyses.

**This study is under review. Information on this page may not be fully correct/finished.**

This research repository contains all material related to our study *"Trial participation in neurodegenerative diseases: barriers and facilitators. A systematic review and meta-analyses."*. 


# What's the project about? # 
There is a pressing need for effective therapies in neurodegenerative diseases, and clinical trials form a key step in the process. However, the proportion of trial participants is low, especially in minority groups. This ultimately leads to limited generalizability and underpowered trial results, both interfering in properly assessing new therapies. By systematically reviewing the literature and employing meta-analyses on created themes, we aimed to identify factors that influence trial participation. By combining a qualitative thematic analysis with a quantitative meta-analysis, we aimed to provide a comprehensive overview of key factors that contribute to trial participation, thereby informing investigators how to better align their recruitment strategies. 


# What is in the repository? #
| Folder     | Content |
|:-----------|:-----------------------------------------------------------------------|
| data       | Contains the data required to run the meta-analysis (`meta.xlsx`) and to create the dynamic flow chart (figure 2; `code tree.xlsx`). |
| figures    | Holds the figures that are available in the manuscript |
| manuscript | Contains the manuscript `TP.pdf` (to be added). |
| review     | Contains all the files related to the coding process. `code tree.xlsx` contains all the emerged enablers (themes) and the quotes related to that enabler. `thematic analysis.xlsx` includes the extracted data from the articles and the (initial) coding process. |
| scripts    | Holds 1) the script required to obtain the results from the meta-analyses, to create the forest plot (figure 4 of manuscript) and the funnel plots (supplement 7) (script: `meta.R`), and 2) the script to create the dynamic flow chart (figure 2; `sankey.R`). |
| supplementary material | Holds the supplementary material in separate files. |


# Machine and package information
```
─ Session info ────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.1 (2022-06-23)
 os       macOS 14.0
 system   x86_64, darwin17.0
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Amsterdam
 date     2023-09-28
 rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
 pandoc   NA
 
 ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
 package     * version    date (UTC) lib source
 boot          1.3-28.1   2022-11-22 [1] CRAN (R 4.2.0)
 cellranger    1.1.0      2016-07-27 [1] CRAN (R 4.2.0)
 cli           3.6.1      2023-03-23 [1] CRAN (R 4.2.0)
 colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.2.0)
 cowplot     * 1.1.1      2020-12-30 [1] CRAN (R 4.2.0)
 dplyr       * 1.1.2      2023-04-20 [1] CRAN (R 4.2.0)
 fansi         1.0.4      2023-01-22 [1] CRAN (R 4.2.0)
 farver        2.1.1      2022-07-06 [1] CRAN (R 4.2.0)
 forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.2.0)
 generics      0.1.3      2022-07-05 [1] CRAN (R 4.2.0)
 ggplot2     * 3.4.2      2023-04-03 [1] CRAN (R 4.2.0)
 ggsankey    * 0.0.99999  2023-05-30 [1] Github (davidsjoberg/ggsankey@3e171a8)
 glue          1.6.2      2022-02-24 [1] CRAN (R 4.2.0)
 gtable        0.3.3      2023-03-21 [1] CRAN (R 4.2.0)
 hms           1.1.3      2023-03-21 [1] CRAN (R 4.2.0)
 labeling      0.4.2      2020-10-20 [1] CRAN (R 4.2.0)
 lattice       0.21-8     2023-04-05 [1] CRAN (R 4.2.0)
 lifecycle     1.0.3      2022-10-07 [1] CRAN (R 4.2.0)
 lme4          1.1-33     2023-04-25 [1] CRAN (R 4.2.0)
 lubridate   * 1.9.2      2023-02-10 [1] CRAN (R 4.2.0)
 magrittr      2.0.3      2022-03-30 [1] CRAN (R 4.2.0)
 MASS          7.3-60     2023-05-04 [1] CRAN (R 4.2.0)
 mathjaxr      1.6-0      2022-02-28 [1] CRAN (R 4.2.0)
 Matrix      * 1.5-4.1    2023-05-18 [1] CRAN (R 4.2.0)
 metadat     * 1.2-0      2022-04-06 [1] CRAN (R 4.2.0)
 metafor     * 4.2-0      2023-05-08 [1] CRAN (R 4.2.0)
 minqa         1.2.5      2022-10-19 [1] CRAN (R 4.2.0)
 munsell       0.5.0      2018-06-12 [1] CRAN (R 4.2.0)
 nlme          3.1-162    2023-01-31 [1] CRAN (R 4.2.0)
 nloptr        2.0.3      2022-05-26 [1] CRAN (R 4.2.0)
 numDeriv    * 2016.8-1.1 2019-06-06 [1] CRAN (R 4.2.0)
 pillar        1.9.0      2023-03-22 [1] CRAN (R 4.2.0)
 pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.2.0)
 purrr       * 1.0.1      2023-01-10 [1] CRAN (R 4.2.0)
 R6            2.5.1      2021-08-19 [1] CRAN (R 4.2.0)
 Rcpp          1.0.10     2023-01-22 [1] CRAN (R 4.2.0)
 readr       * 2.1.4      2023-02-10 [1] CRAN (R 4.2.0)
 readxl      * 1.4.2      2023-02-09 [1] CRAN (R 4.2.0)
 rlang         1.1.1      2023-04-28 [1] CRAN (R 4.2.0)
 rstudioapi    0.14       2022-08-22 [1] CRAN (R 4.2.0)
 scales        1.2.1      2022-08-20 [1] CRAN (R 4.2.0)
 sessioninfo * 1.2.2      2021-12-06 [1] CRAN (R 4.2.0)
 stringi       1.7.12     2023-01-11 [1] CRAN (R 4.2.0)
 stringr     * 1.5.0      2022-12-02 [1] CRAN (R 4.2.0)
 tibble      * 3.2.1      2023-03-20 [1] CRAN (R 4.2.0)
 tidyr       * 1.3.0      2023-01-24 [1] CRAN (R 4.2.0)
 tidyselect    1.2.0      2022-10-10 [1] CRAN (R 4.2.0)
 tidyverse   * 2.0.0      2023-02-22 [1] CRAN (R 4.2.0)
 timechange    0.2.0      2023-01-11 [1] CRAN (R 4.2.0)
 tzdb          0.4.0      2023-05-12 [1] CRAN (R 4.2.0)
 utf8          1.2.3      2023-01-31 [1] CRAN (R 4.2.0)
 vctrs         0.6.2      2023-04-19 [1] CRAN (R 4.2.0)
 withr         2.5.0      2022-03-03 [1] CRAN (R 4.2.0)
 zoo         * 1.8-12     2023-04-13 [1] CRAN (R 4.2.0)

 [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library
```
 
# Access and permissions
The creation and maintenance of this research repository are the responsibilities of the author (Daphne Weemering). This archive is completely open access, and can be accessed for an indefinite period.


# Contact 
You can reach me via e-mail at <d.n.weemering@umcutrecht.nl> or Ruben van Eijk (corresponding author) at <r.p.a.vaneijk-2@umcutrecht.nl>. 








