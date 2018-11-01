

# yspec YAML specification

- `about: [short-name, units]`
- `long: a longer name to describe the column`
- `range: [min-value, max-value]`
    - indicates continuous data
- `values: [val1, val2, valn]`
    - specify eacy valid value
    - indicates discrete data
- `values: {name1: val1, name2: val2}`
    - put the decode into the specification of values
    - __Note__ the curly brackets, not square brackets
- `longvalues: true`
    - print the values in a (long) `yaml`-formatted list
- `decode: [name1, name2, name3]`
    - Separate the `decode` from the `values` specification
    - See example below for clearer way to input very long decodes
- `comment: just whatever you want to say`
- `comment: >
      say something
      on multiple lines of
      text`
- `source: ADSL.xpt`
    - Where the data came from
    - include both the sdtm domain and variable name
- `derivation: WEEKS = TIME/24/7`
- `if_missing: imputed`


## Examples

### Continuous values

- `The about` array provides a short name and units
- Using `range` indicates continuous data

```yaml
WT:
  about: [Weight, kg]
  range: [5,300]
```

### Character data
- Using `values` indicates discrete data

```yaml
RACE:
  values: [white, black, Native American, Other]
```

By default, `values` are printed as comma-separated list.  To
get them to print in long format

```yaml
RACE:
  values: [white, black, Native American, Other]
  longvalues: true
```

### Integer data with decode

__Method 1__

- Notice that the yaml key can only be simple character value
- Also, we use curly braces to specify a list like this
- Finally it is a `:` that separates name and value (not `male=1`)

```yaml
SEX:
  values: {dude: 0, gal: 1}
```

__Method 2__

- These are more complicated decodes
- Put the `values` and `deode` in brackets (array)

```yaml
BQL:
  values: [0,1]
  decode: [not below quantitation limit, below quantitation limit]
```

__Method 3__
Really, it's the same as method 2, but easier to type and read when the
decode gets really long

```yaml
BQL:
  values [0, 1]
  decode:
    - not below the quantitation limit of 2 ng/ml
    - below the quantitation limit of 2 ng/ml
```


### Split data column

- `DV` is on type on some records different type on other records
- This happens when you have PK/PD data set or parent / metabolite
- Use the `split` field to specify the different `DV` types
- Split needs curly brackets `{}`
- Specify an `about` field as normal
- Also specify `when` ... a condition in the other data columns
that brings this definition in the mix


```yaml
DV:
  long: dependent variable
  split:
    DV1: {about: [concentration, ng/mL], when: SEQ==1}
    DV2: {about: itch level, when: SEQ==2}
    DV3: {about: [heart rate, bpm], when: SEQ==3}
comment: see `SEQ`
```
