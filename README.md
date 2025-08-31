# Master-Thesis

Welcome to this interactive dashboard, which allows users to navigate through different graphs that visualize in a synthetic way what was being debated in the Spanish Parliament during the XII Legislative term. 

Using data from the [ParlSpeech](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L4OAKN) database, I created an R Shiny web application that analyses the deputies’ interventions between 2016 and 2018 applying different text mining techniques such as TF-IDF, topic modelling, sentiment analysis and more.

This repository includes three folders:

  - The **Notebooks** folder includes different Quarto files explaining how the text was cleaned and pre-process for the analysis and an overview of the main text mining techniques we used for the dashboard.
  - The **DATA** folder includes the the lexicon files as well as the .csv with the deputies’ names. If you want to access the original corpus dataset, you need to download it from the [ParlSpeech](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L4OAKN) website, because GitHub does not allow to upload files over 100MB.
  - The **TFM Dashboard** includes the necessary data files to run the web application and the application script.
