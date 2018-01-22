# qtoolkit
R package to process and visualize Qualtrics survey data via the Qualtrics API

### Quick Start

```
## Install qtoolkit (requires `devtools` package)
install.packages("devtools")
devtools::install_github("earthlab/qtoolkit")

## Connect to Qualtrics API
> qapi_connect(org_id  = "OrganizationID",
               api_key = "abcdefghijklmnopqrstuvwxyz")
               
## List surveys
> list_surveys()

+-------------------+-----------------+-------------------+---------------------+-----------+
|        id         |      name       |      ownerId      |    lastModified     | isActive  |
+-------------------+-----------------+-------------------+---------------------+-----------+
| SV_aBcDeFgHiJkLmN | Customer Survey | UR_AbCdEfGhIjkLmN | 2017-12-1222:17:40Z | TRUE      |
+-------------------+-----------------+-------------------+---------------------+-----------+
| SV_ZyXwVuTsRqPoNm | Employee Survey | UR_zYxWvUtSrQpOnM | 2017-12-3010:20:22Z | FALSE     |
+-------------------+-----------------+-------------------+---------------------+-----------+

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
