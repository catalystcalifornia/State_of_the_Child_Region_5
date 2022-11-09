# State_of_the_Child_Region_5
Draft Findings and Visuals for the State of the Child Region 5 Report.

 <li>
      <a href="https://advancementprojectca-rda.github.io/State_of_the_Child_Region_5/findings_visuals_draft.html"> Draft Findings HTML Link<br>https://advancementprojectca-rda.github.io/State_of_the_Child_Region_5/findings_visuals_draft.html</a>
      </li>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>


## About The Project

The State of the Child Antelope Valley Report seeks to understand the conditions of children and families within this current moment in the Antelope Valley, California Best Start Region 5. It encompasses topics including demographics, education, economic wellbeing, etc.

<p align="right">(<a href="#top">back to top</a>)</p>


### Data Source(s)
# *[American Community Survey 2016-2020 5-year estimates calculated for Antelope Valley neighborhoods by Lucy Wilkerson](https://public.tableau.com/app/profile/luz3725/viz/2020CensusData-AVBESTSTARTREGION5/GRAPI)
# *[2019 US Census American Community Survey (ACS) 1-year Estimates, Table S0201](https://data.census.gov/cedsci/table?q=S0201&t=-00%20-%20All%20available%20races&g=0400000US06%240500000&tid=ACSSPP1Y2019.S0201)
# *[Children's Data Network Infant Mortality Rates](https://www.first5la.org/wp-content/uploads/2020/09/First-5-LA-2020-Indicators-Report.pdf)
# *[California Department of Public Health](https://data.chhs.ca.gov/dataset/infant-mortality-deaths-per-1000-live-births-lghc-indicator-01/resource/ae78da8f-1661-45f6-b2d0-1014857d16e3)
# *[African American Infant and Maternal Mortality (AAIMM) Initiative](https://www.blackinfantsandfamilies.org/)
# *[A Pathway to Equity](http://publichealth.lacounty.gov/centerforhealthequity/PDF/AAIM-ActionPlan.pdf)
# *[Vera Institute for Justice](https://www.vera.org/)
# *[California Department of Education, 2015-2022](https://dq.cde.ca.gov/dataquest/)
# *[Los Angeles Homeless Services Authority, 2022](https://www.lahsa.org/data)
# *[United Ways of California Real Cost Measure, 2021](https://www.unitedwaysca.org/realcost/39-real-cost)
# *[L.A. County WIC Administrative Data, 2021](https://lawicdata.org/data-research/by-region/)

<p align="right">(<a href="#top">back to top</a>)</p>

### Built with
<img-align="left" alt="R" width="32px", src="https://upload.wikimedia.org/wikipedia/commons/1/1b/R_logo.svg"/><img-align="left" alt="RStudio" width="32px", src="https://upload.wikimedia.org/wikipedia/commons/d/d0/RStudio_logo_flat.svg"/>


<p align="right">(<a href="#top">back to top</a>)</p>


<!-- GETTING STARTED -->
## Getting Started

<!--This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps. -->

### Prerequisites

The data cleaning, analysis, and visualization was conducted with the following software. In addition to that, there are several R packages that were used to pull Census data and perform different functions.
* [R](https://cran.rstudio.com/)
* [RStudio](https://posit.co/download/rstudio-desktop/)


### Installation

1. Get a free TidyCensus API Key at [https://walker-data.com/tidycensus/articles/basic-usage.html](https://walker-data.com/tidycensus/articles/basic-usage.html)
2. Clone the repo
   ```sh
   git clone https://github.com/github_username/repo_name.git
   ```
3. Install R packages
* dplyr
* tidyr
* ggplot2
* stringr
* sf
* data.table
* janitor
* ggtext
* kableExtra
* RPostgreSQL
* usethis


  ```r
list.of.packages <- c("openxlsx","tidycensus", "tidyr","dplyr","stringr","RPostgreSQL","data.table", "kableExtra", "ggplot2", "ggtext", "janitor","sf") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("r-lib/usethis")

library(openxlsx)
library(stringr)
library(tidycensus)
library(tidyr)
library(dplyr)
library(RPostgreSQL)
library(data.table)
library(sf)
library(janitor)
library(kableExtra)
library(ggplot2)
library(ggtext)
library(usethis)
```

4. Enter your API in `.Renviron` File for Repeated Use
[https://walker-data.com/tidycensus/reference/census_api_key.html](https://walker-data.com/tidycensus/reference/census_api_key.html)

```r
if (FALSE) {
census_api_key("111111abc", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")
}

if (FALSE) {
# If you need to overwrite an existing key:
census_api_key("111111abc", overwrite = TRUE, install = TRUE)
# First time, relead your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")
}

```

<p align="right">(<a href="#top">back to top</a>)</p>


<!--## Usage

Use this space to show useful examples of how a project can be used (e.g. iframes, citation, etc). Additional screenshots, code examples and demos work well in this space. You may also link to more resources.-->


<p align="right">(<a href="#top">back to top</a>)</p>


<!--## Roadmap

Use this space to list out future goals for this project (if any)
- [ ] Feature 1
- [ ] Feature 2
- [ ] Feature 3
    - [ ] Nested Feature-->

<p align="right">(<a href="#top">back to top</a>)</p>


## Contributors

* [Alexandra Baker](https://github.com/bakeralexan)
* [Chris Ringewald](https://github.com/cringewald)
* [David Segovia](https://github.com/davidseg1997)
* [Maria Khan](https://github.com/mariatkhan)



<p align="right">(<a href="#top">back to top</a>)</p>


## Contact

<!--Use this space to add a contact for questions/concerns that visitors may have-->

[Chris Ringewald](https://www.linkedin.com/in/chris-ringewald-6766369/) - cringewald@catalystcalifornia.org
[Alexandra Baker](https://www.linkedin.com/in/alexandra-baker-84696075/) - abaker@catalystcalifornia.org

Project Link: [https://github.com/advancementprojectca-rda/State_of_the_Child_Region_5](https://github.com/advancementprojectca-rda/State_of_the_Child_Region_5)

<p align="right">(<a href="#top">back to top</a>)</p>
<!--

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.-->

<p align="right">(<a href="#top">back to top</a>)</p>


## Acknowledgments
<!--Use this space for any additional acknowledgments (project partners, etc)-->

* [Children's Bureau](https://www.all4kids.org/)
* [Tracey La Monica](traceylamonica@all4kids.org)
* [Sylvia Scott](sylviascott@all4kids.org)

<p align="right">(<a href="#top">back to top</a>)</p>
