[![DOI](https://zenodo.org/badge/107568212.svg)](https://zenodo.org/badge/latestdoi/107568212)

# qtoolkit

Qtoolkit is an `R` package that makes it easier to get, process and visualize Qualtrics survey data. `Qtoolkit` allows you to connect to the
Qualtrics API in order to download surveys, view survey questions and associated metadata and quickly analyze and plot the data outputs.

## Get API Data

To access the Qualtrics API, you need to do the following:

1. Find your `baseurl` and API token.
2. Add both items information to your `.Rprofile` file.

Once you do this, you can use the qualtrics API from any working directory.


### Organization ID

FIRST, you need the Organization ID which is the "server" space where your version of
qualtrics lives. The url looks something like:
`https://yourdomainhere.qualtrics.com`. For CU it's

Example: https://cuboulder.qualtrics.com

Follow the [Base URL and Datacenter IDs baseurl documentation](https://api.qualtrics.com/docs/root-url) to find the base url of
your qualtrics account.

Important - `qtoolkit` only requests your domain and will generate the full url
for you. So following the example above, you only need to add `cuboulder` to
your `.Rprofile` file.


### The API Token

Qualtrics will provide you an API token that is specifically associated with YOUR
ACCOUNT within the qualtrics organization. This is required to authenticate
and access surveys in your account and to access surveys in your account.

Follow the [API Token Generation Instructions](https://api.qualtrics.com/docs/authentication-1) here to generate
the API for your account.


### Update Your .Rprofile File

Once you have accessed your organization id and API Token, you are ready to
authenticate. The preferred method of doing this is to add the text below (replacing
it with your OrganizationId and api keys) to your `.Rprofile` file.

The `.Rprofile` file is located in your HOME DIRECTORY of your computer.
If you are using bash, you can use `cd ~` to quickly navigate to your home directory.
Then open `.Rprofile` in your favorite text editor and add the following:

```
options(QAPI_ORG_ID  = "OrganizationId",
        QAPI_API_KEY = "abcdefghijklmnopqrstuvwxyz")
```

Check out the vignette on authentication to read about the other ways to pass
your key to qualtrics.

## Start Using qtoolkit

Once you have the authentication information above, you are read to work with `Qtoolkit`.
First, install the package from the Earth Lab github organization.


```
## Install qtoolkit (requires `devtools` package)
install.packages("devtools")
devtools::install_github("earthlab/qtoolkit")
```

Next, connect to the api. If you added the authentication to your `.Rprofile` file
following the instructions above, you can connect as follows:

```
## Connect to Qualtrics API
qapi_connect()
```

If that information is not in the `.Rprofile` file, then you can manually connect
by typing in your organization id and api_key. WARNING: do not commit this information
into version control or share it with anyone! We highly suggest that you use
the `.Rprofile` instructions above!

```
## Connect to Qualtrics API
> qapi_connect(org_id  = "OrganizationID",
               api_key = "abcdefghijklmnopqrstuvwxyz")
```

Once you have connected to the API, you can view all of the surveys in your account.
`list_surveys()` returns a `data.frame` with the survey id, name and a few other
key attributes.

```
## List surveys
> list_surveys()

+-------------------+-----------------+-------------------+---------------------+-----------+
|        id         |      name       |      ownerId      |    lastModified     | isActive  |
+-------------------+-----------------+-------------------+---------------------+-----------+
| SV_aBcDeFgHiJkLmN | Customer Survey | UR_AbCdEfGhIjkLmN | 2017-12-1222:17:40Z | TRUE      |
+-------------------+-----------------+-------------------+---------------------+-----------+
| SV_ZyXwVuTsRqPoNm | Employee Survey | UR_zYxWvUtSrQpOnM | 2017-12-3010:20:22Z | FALSE     |
+-------------------+-----------------+-------------------+---------------------+-----------+
```
Once you know what survey you want, you can access it using `qsurvey()`. qsurvey()
returns a survey object with all of the wonderful metadata that you could ever want
to process, clean up and visualize your data.

**LEAH NOTE: I think this should be get_survey()**

```
## Get survey
> sv <- qsurvey("SV_aBcDeFgHiJkLmN")

## List survey questions
> sv$questionList

+----+-------+------+-------+-------------+------+----------+-------------+----------+----------+
|    |  qid  | name | order |    text     | type | selector | subselector | required |  label   |
+----+-------+------+-------+-------------+------+----------+-------------+----------+----------+
|  1 | QID15 | Q1   |     1 | Question 1  | DB   | TB       | NA          | FALSE    | Label 1  |
|  2 | QID18 | Q2   |     2 | Question 2  | DB   | TB       | NA          | FALSE    | Label 2  |
|  3 | QID36 | Q37  |     3 | Question 3  | TE   | SL       | NA          | FALSE    | Label 3  |
|  4 | QID38 | Q379 |     4 | Question 4  | MC   | SAVR     | NA          | FALSE    | Label 4  |
|  5 | QID37 | Q36  |     5 | Question 5  | MC   | MAVR     | NA          | FALSE    | Label 5  |
|  6 | QID1  | Q3   |     6 | Question 6  | MC   | SAVR     | NA          | FALSE    | Label 6  |
|  7 | QID31 | Q30  |     7 | Question 7  | MC   | MAVR     | NA          | FALSE    | Label 7  |
|  8 | QID23 | Q26  |     8 | Question 8  | MC   | MAVR     | NA          | FALSE    | Label 8  |
|  9 | QID34 | Q99  |     9 | Question 9  | MC   | MAVR     | NA          | FALSE    | Label 9  |
| 10 | QID2  | Q4   |    10 | Question 10 | MC   | SAVR     | NA          | FALSE    | Label 10 |
+----+-------+------+-------+-------------+------+----------+-------------+----------+----------+

## Get a particular question by QID
> sv$questions$QID15
```
