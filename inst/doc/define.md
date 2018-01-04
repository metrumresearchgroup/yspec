Data source
-----------

-   Data Set: `spec`
-   Date: 2018-01-03

Data definitions
----------------

1.  **`C`**
    -   commented rows
    -   numeric
    -   values: `.`, `C`
2.  **`USUBJID`**
    -   universal subject identifier
    -   numeric
3.  **`ID`**
    -   NONMEM ID number
    -   numeric
4.  **`EVID`**
    -   event ID indicator
    -   numeric
    -   comment: per NONMEM specifications
5.  **`MDV`**
    -   missing DV indicator
    -   numeric
    -   comment: per NONMEM specifications
6.  **`SEQ`**
    -   record type indicators
    -   numeric
    -   values: `1 = concentration`, `2 = response`, `3 = other`
7.  **`AMT`**
    -   dose amount
    -   short name: Amount
    -   numeric
    -   unit: `mg`
8.  **`II`**
    -   interdose interval
    -   numeric
    -   unit: `hours`
9.  **`CMT`**
    -   compartment number
    -   short name: Compartment
    -   numeric
    -   values: `1`, `2`, `3`, `4`
    -   comment: per NONMEM specifications
10. **`TAFD`**
    -   time after first dose
    -   numeric
    -   unit: `hours`
11. **`WT`**
    -   baseline weight
    -   short name: Weight
    -   numeric
    -   unit: `kg`
12. **`EGFR`**
    -   estimated glomerular filtration rate
    -   short name: eGFR
    -   numeric
    -   unit: `ml/min/1.73 m2`
    -   if\_missing: imputed
13. **`STUDY`**
    -   study identifier
    -   short name: Study
    -   numeric
    -   values:
        -   `100 = The first phase 1 study`
        -   `202 = The first phase 2 study conducted only at sites 1, 2, 3 when the formulation was still oral liquid only`
        -   `303 = The renal impairment study`
        -   `203 = The second phase 2 study`
14. **`SEX`**
    -   numeric
    -   values: `0 = male`, `1 = female`
15. **`BQL`**
    -   DV value is below the quantitation limit
    -   short name: below limit of quantification
    -   numeric
    -   values: `0 = FALSE`, `1 = TRUE`
16. **`DV`**
    -   dependent variable
    -   numeric
        -   NoDoze concentration `ng/mL` when `SEQ==1`
        -   Alertness `NULL` when `SEQ==2`
        -   BMI `kg/m2` when `SEQ==3`
    -   comment: see SEQ
17. **`DV2`**
    -   second dependent variable
    -   numeric
        -   NoDoze concentration `ng/mL`
        -   Alertness `NULL`
        -   BMI `kg/m2`
18. **`HAIR`**
    -   patient hair color
    -   numeric
    -   values: `0 = brown`, `1 = blonde`, `2 = black`
19. **`CLCR`**
    -   short name: creatinine clearance
    -   numeric
    -   unit: `ml/min`
    -   if\_missing: dropped
