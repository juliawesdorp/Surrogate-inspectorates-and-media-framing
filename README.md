# Surrogate-inspectorates-and-media-framing
This project is called "The influence of surrogate inspectorates on media framing of regulatory agencies". The study is part of the project "Authoritative reputations of regulatory agencies", a collaboration between Erasmus University Rotterdam and Utrecht University. This specific study uses a quantitative content analysis of 2700 Dutch news articles to study the effect of so-called surrogate inspectorates on the way regulatory agencies are being framed in the media.

**Data**
The data is available in .csv and .xlsx format.

**The following R packages are used:**
- dplyr (version 1.1.4)
- lubridate (version 1.9.3)
- ggplot2 (version 3.5.1)
- car (version 3.1.3)
- DescTools (version 0.99.58) 

**To start:**
# Import data
dataset <- read.csv("path/to/your/file.csv", stringsAsFactors=FALSE, sep= ",")

The following  numbers belong to these regulatory agencies:
1 = Nederlandse Voedsel- en Warenautoriteit
2 = Inspectie Leefomgeving en Transport
3 = Autoriteit Consument & Markt
4 = Staatstoezicht op de Mijnen
5 = Milieudinest Rijnmond## 5 = DCMR
8 = Autoriteit Persoongsgevens
Number 6 and 7 were not used in this study.

License: Apache License 2.0
