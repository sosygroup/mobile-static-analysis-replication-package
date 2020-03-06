# Mobile static analysis replication-package
Replication package of the systematic mapping study on static analysis techniques for mobile apps.

This study has been designed, developed, and reported by the following investigators:
- [Marco Autili](http://people.disim.univaq.it/marco.autili/) (University of L'Aquila, Italy)
- [Ivano Malavolta](http://www.ivanomalavolta.com) (Vrije Universiteit Amsterdam, The Netherlands)
- [Alexander Perucci](http://www.alexanderperucci.com/) (University of L'Aquila, Italy)
- [Gian Luca Scoccia](https://scholar.google.com/citations?user=y8EX4DAAAAAJ&hl=en) (University of L'Aquila, Italy)
- [Roberto Verdecchia](https://robertoverdecchia.github.io/) (Gran Sasso Science Institute, Italy | Vrije Universiteit Amsterdam, The Netherlands)

For any information, interested researchers can contact us by writing an email to [i.malavolta@vu.nl](mailto:i.malavolta@vu.nl)

## Structure of the replication package

This replication package is organized according to the following structure.

```
Root
│   readme.md                    # the file you are reading right now.
│   protocol.pdf                 # a document containing the review protocol we followed for executing the whole study.
|   analysis                     # a folder containing exploration plots, raw data, and analysis scripts
|─── output                      # a folder containing the working plots we used for exploring the extracted data
|─── |─── summary.pdf            # a PDF file containing a bar plot for each parameter of the data extraction form
|─── |─── horizontal.pdf         # a PDF file containing the contingency tables we used for the horizontal analysis
|─── rawData                     # a folder containing the raw data extracted from each primary study.
|─── |─── data.xlsx              # an Excel spreadsheet containing all the extracted data. There you will find a colum for each parameter of the data extraction form and a row for each primary study.
|─── |─── data.csv               # a comma-separated-value textual file created by exporting the extractedData.xlsx Excel spreadsheet 
└──  scripts                     # a folder containing all the R scripts for analysing the extracted data and for generating all the figures included in the paper. Each script is self-contained, it does not depend on other scripts, and has been executed via R Studio. 
```
