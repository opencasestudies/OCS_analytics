# OCS_analytics
Repository for analyzing [`OCSdata`](https://github.com/opencasestudies/OCSdata) package downloads, [OCS survey](https://docs.google.com/forms/d/e/1FAIpQLSfpN4FN3KELqBNEgf2Atpi7Wy7Nqy2beSkFQINL7Y5sAMV5_w/viewform) reviews and feedback, and Google Analytics web traffic and engagement metrics.

## Data
### Survey
Survey data from Google Forms is automatically saved to a Google Sheets spreadsheet. Data from this spreadsheet is scraped by interfacing with the Sheets API using the using the  `read_sheets` function from [`googlesheets4`](https://googlesheets4.tidyverse.org/). 

### Google Analytics
Data is extracted from Google Analytics by interfacing with the GA4 API using the [`googleAnalyticsR`](https://github.com/8-bit-sheep/googleAnalyticsR/) package. 

### OCSdata Downloads
`OCSdata` package downloads data are extracted using the ['packageRank'](https://github.com/lindbrook/packageRank/tree/master) package and visualized using the [`Visualize.CRAN.Downloads`](https://github.com/mponce0/Visualize.CRAN.Downloads) package. 

## Publications
Results from this analysis are reported in [@mbreshock](https://github.com/mbreshock)'s Master's thesis:

Breshock, Michael. (2021). *Expanding access and removing barriers: Data science education with the Open Case Studies digital platform* [Thesis, Johns Hopkins University]. [jscholarship.library.jhu.edu/handle/1774.2/66820](https://jscholarship.library.jhu.edu/handle/1774.2/66820)

More recently, results from this analysis were also published in the [Journal of Statistics and Data Science Education](https://www.tandfonline.com/journals/ujse21):

Wright C, Meng Q, Breshock MR, Atta L, Taub MA, Jager LR, Muschelli J, Hicks SC. (2024). *Open Case Studies: Statistics and Data Science Education through Real-World Applications.* Journal of Statistics and Data Science Education. DOI: [10.1080/26939169.2024.2394541](https://doi.org/10.1080/26939169.2024.2394541).
