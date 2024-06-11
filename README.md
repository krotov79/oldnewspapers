# oldnewspapers
Old Newspapers Analysis
This project analyzes the Old Newspapers dataset to uncover trends, sentiments, and relationships between words over time.
Prerequisites
Kaggle API Key:
Go to your Kaggle account and create a new API token. This will download a kaggle.json file containing your credentials.
Place the kaggle.json file in the root directory of this project.

Running the Analysis
Set Up the Environment:
Source the setup.R script to set up the environment and download the dataset.
Run the Analysis:
Execute the main analysis script.
Analysis Overview
The project consists of several key steps:

Data Cleaning and Preprocessing:

Filtering English articles
Handling missing data
Text normalization
Tokenization and removal of stop words
Data Exploration and Visualization:

Temporal distribution analysis
Initial word frequency analysis
Modeling Approach:

Sentiment analysis using different lexicons
Topic modeling using Latent Dirichlet Allocation (LDA)
Exploring relationships between words using n-grams
Word embeddings and t-SNE visualization
Results
The analysis includes insights such as:

Trends in article publication over time
Sentiment trends and comparisons using different lexicons
Key topics identified in the articles
Relationships between words and semantic similarities
Contributions
Contributions to this project are welcome. If you have any ideas for improvements or new analyses, please feel free to submit a pull request or open an issue.

References
AFINN lexicon from Finn Årup Nielsen. URL: https://github.com/fnielsen/afinn
Alvations, S. (2019). Old Newspapers Dataset. Kaggle. Retrieved from https://www.kaggle.com/datasets/alvations/old-newspapers
bing lexicon from Bing Liu and collaborators. URL: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
Jockers, M. (2021). Syuzhet: Extract Sentiment and Plot Arcs from Text. R package version 1.0.6. https://cran.r-project.org/web/packages/syuzhet/index.html
nrc lexicon from Saif Mohammad and Peter Turney. URL: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
Silge, J., & Robinson, D. (2017). Text Mining with R: A Tidy Approach. O'Reilly Media, Inc. URL: https://www.tidytextmining.com/
