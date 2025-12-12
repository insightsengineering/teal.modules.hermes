# Helper Function to Update Gene Selection

This helper function takes the intersection of `selected` and `choices`
for genes and updates the `inputId` accordingly. It then shows a
notification if not all `selected` genes were available.

## Usage

``` r
h_update_gene_selection(session, inputId, selected, choices)
```

## Arguments

- session:

  (`ShinySession`)  
  the session object.

- inputId:

  (`string`)  
  the ID used to call the module input.

- selected:

  (`character`)  
  currently selected gene IDs.

- choices:

  (`data.frame`)  
  containing `id` and `name` columns of the new choices.
