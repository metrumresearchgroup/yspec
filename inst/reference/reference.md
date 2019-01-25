

# yspec data specification

## Data fields

- `short`: a short name for the column; don't include units
- `units`: the measurement unit
- `type:` the data type; can be either `numeric` (default) or `character`
- `lookup:`
    - could be logical `lookup: true`
    - could be the name of a lookup column to use: `lookup: ALTEGFR`
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
- `about: [short, units]` just a short cut
- `long: a longer name to describe the column`
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

## Handlers

- `!look`: shortcut for `lookup: true`
- `!decode:value`: when coding a map, put the `decode` part to the 
  left of the `:` and the `value` to the right
- `!value:decode`: the reverse of `!decode:value` (e.g. the `value` is 
  on the left

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

### Discrete data with decode

__Method 1__

- Notice that the yaml key can only be simple character value
- Also, we use curly braces to specify a list like this
- Finally it is a `:` that separates name and value (not `male=1`)

```yaml
SEX:
  values: {dude: 0, gal: 1}
```

This can also be written like

```yaml
SEX: 
  dude: 0
  gal: 1
```

In the above examples, the pattern is `decode:value`.  We can make this 
explicity by using the `!decode:value` handler

```yaml
SEX: !decode:value
  dude: 0
  gal: 1
```

And we can have the reverse pattern (`value` on the left) as well with

```yaml
SEX: !decode:value
  0 : dude
  1 : gal
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
