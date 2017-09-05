Data source
-----------

-   File: tran2.xpt
-   Date: 2017-09-05

Data definitions
----------------

1.  **`C`**
    -   commented rows
    -   values: `.`, `C`
2.  **`USUBJID`**
    -   universal subject identifier
3.  **`ID`**
    -   NONMEM ID number
4.  **`EVID`**
    -   event ID indicator
    -   comment: per NONMEM specifications
5.  **`MDV`**
    -   missing DV indicator
    -   comment: per NONMEM specifications
6.  **`SEQ`**
    -   record type indicators
    -   values: `1 = concentration`, `2 = response`, `3 = other`
7.  **`AMT`**
    -   dose amount
    -   unit: `mg`
8.  **`II`**
    -   interdose interval
    -   unit: `hours`
9.  **`CMT`**
    -   compartment number
    -   short name: Compartment
    -   values: `1`, `2`, `3`, `4`
    -   comment: per NONMEM specifications
10. **`TAFD`**
    -   time after first dose
    -   unit: `hours`
11. **`WT`**
    -   baseline weight
    -   short name: Weight
    -   unit: `kg`
12. **`EGFR`**
    -   estimated glomerular filtration rate
    -   short name: eGFR
    -   unit: `ml/min/1.73 m2`
13. **`STUDY`**
    -   study identifier
    -   short name: Study
    -   values:
        -   `100 = The first phase 1 study`
        -   `202 = The first phase 2 study conducted only at sites 1, 2, 3 when the formulation was still oral liquid only`
        -   `303 = The renal impairment study`
        -   `203 = The second phase 2 study`
14. **`SEX`**
    -   values: `0 = male`, `1 = female`
15. **`BQL`**
    -   DV value is below the quantitation limit
    -   short name: below limit of quantification
    -   values: `0 = FALSE`, `1 = TRUE`
16. **`DV`**
    -   dependent variable
        -   NoDoze concentration `ng/mL` when `SEQ==1`
        -   Alertness when `SEQ==2`
        -   BMI `kg/m2` when `SEQ==3`
    -   comment: see SEQ
17. **`DV2`**
    -   second dependent variable
        -   NoDoze concentration `ng/mL`
        -   Alertness
        -   BMI `kg/m2`
18. **`HAIR`**
    -   patient hair color
    -   values: `0 = brown`, `1 = blonde`, `2 = black`
19. **`CLCR`**
    -   short name: creatinine clearance
    -   unit: `ml/min`
    -   derivation: `(140-AGE)*IBW/SCR`; `x 0.85 if female`
