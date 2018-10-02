[![Build Status](https://travis-ci.org/obiii/Lab5.svg?branch=master)](https://travis-ci.org/obiii/Lab5)

## Viser
This package uses **data.motogomery** API to get and analayse crimme data for mmontgomery county.

The package contains the following functionalities:

* #### getLimitedData(limit = 500):
    > Fetches the crime data limited by variable **limit** as function argument.

* #### getDataByCrimeType(dataset,crimetype):
    > Fetches the crime data for a specific crime type given as integer as function argument.

* #### cleanData():
    > Cleans the data i.e. format dates, remove useless variables

* #### getCrimeByID(id):
    > Returns the crime type.


* #### Vignettes(id):
    > browseVignettes("viser")
