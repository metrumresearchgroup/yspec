

# yspec Data Specification


## Data specification configuration field

Include a field called `SETUP__` in the yaml 
document to give settings for the specification.

For a data set specification document, this most likely 
means naming another  yaml file that contains specification
entries to be imported into the current document. 

For example, 

```yaml
SETUP__:
  lookup_file: dictionary.yml
```

will tell yspec to read from the file `dictionary.yml` (always 
assumed to be in the same directory as the 
data specification file) and use those dictionary 
entries to  


## Data specification fields

Use these fields for documenting columns in a data set

- `about`: [short-name, units]
- `long`: a longer name to describe the column`
- `range`: [min-value, max-value]
    - indicates continuous data
- `values`: [val1, val2, valn]
    - specify eacy valid value
    - indicates discrete data
- `values`: {name1: val1, name2: val2}
    - put the decode into the specification of values
    - __Note__ the curly brackets, not square brackets
- `longvalues`: true
    - print the values in a (long) yaml-formatted list
- `decode`: [name1, name2, name3]
    - Separate the `decode` from the `values` specification
    - See example below for clearer way to input very long decodes
- `comment`: just whatever you want to say
- `source`: ADSL.xpt
    - Where the data came from
    - Include both the sdtm domain and variable name
- `dots`: 
  - A named list of whatever you want to carry along in the object; the `dots` list
    isn't used by any rendering function in the yspec package, but might be used 
    by a custom rendering function 
- `axis`:
  - A short-ish name that can be used for axis titles for plots
  - Generally, don't include units; yspec helpers will add that 
    automatically by default
  - If `short` will work for your axis title (as it is ... with no 
    modification), yspec will use that if no `axis` field is used

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

Because yaml is so cool, you can write the same thing like this

```yaml
SEX: 
  values:
    dude: 0
    gal: 1
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

### A long comment

```yaml
comment: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque 
         at iaculis ligula. Nunc volutpat justo sapien, sit amet aliquet 
         magna aliquet quis. Phasellus ut dignissim ante. Fusce mattis at 
         ante a suscipit.  Cras in accumsan arcu. Integer tristique nisl nec 
         metus porttitor, eu tincidunt nibh aliquet. Sed placerat nisi 
         condimentum lacus facilisis, et sollicitudin purus luctus.
```

or

```yaml
comment: >
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque 
  at iaculis ligula. Nunc volutpat justo sapien, sit amet aliquet magna 
  aliquet quis. Phasellus ut dignissim ante. Fusce mattis at ante a 
  suscipit. Cras in accumsan arcu. Integer tristique nisl nec metus 
  porttitor, eu tincidunt nibh aliquet. Sed placerat nisi condimentum 
  lacus facilisis, et sollicitudin purus luctus.
```



### Look up column specification information from a dictionary


```yaml
SETUP___:
  lookup_file: dictionary.yml
WT:
  short: Weight
  unit: kg
  source: Patient demographics data set
EGFR:
  lookup: true
CA:
  lookup: corrected_calcium
ALB:
  unit: mg/gallon
  lookup: true
```

In the above example

  - We name a lookup file in `SETUP__` so we can import standard definitions
  - `WT` is taken as specified in the listing
  - `EGFR` is imported fully from the `EGFR` entry in `dictionary.yml`
  - `CA` is imported from the `corrected_calcium` listing in `dictionary.yml`; but 
    the column name remains `CA`
  - `ALB` is imported using all fields from `dictionary.yml` except for 
     `unit` which is taken as `mg/gallon`





# The project specification file

When there are multiple data sets and data specification files
that need to get rendered in a single document, make 
another yaml file that names and describes the data sets in 
a project.  

A project file might look like

```yaml
SETUP__:
  data_path: "../data/derived"
DEMO101101_PK:
  description: The population PK analysis data set
DEMO101101_PD:
  description: The population PD analysis data set
```

This project file names two data sets (DEMO101101_PK and DEMO101101_PD) and 
associates a description with each data set. 

The `data_path` field in the `SETUP__` block gets applied to 
both data sets or you can give a different `data_path` to each 
data set.  

Use either `render_define` or `render_fda_define` functions
to create a project-wide, data specification document (in this case, 
listing definitions for both data sets in the analysis).



