# Module Input for Gene Signature Specification

This defines the input for the gene signature specification.

## Usage

``` r
geneSpecInput(
  inputId,
  funs,
  label_genes = "Select Gene(s)",
  label_funs = "Select Gene Summary",
  label_text_button = "Enter list of genes",
  label_lock_button =
    "Lock gene selection (so that it does not get updated when filtering)",
  label_select_all_button = paste0("Select All Genes (first ", max_options, ")"),
  label_select_none_button = "Select None",
  max_options = 200L,
  max_selected = max_options
)
```

## Arguments

- inputId:

  (`string`)  
  the ID used to call the module input.

- funs:

  (named `list`)  
  names of this list will be used for the function selection drop down
  menu.

- label_genes:

  (`string`)  
  label for the gene selection.

- label_funs:

  (`string`)  
  label for the function selection.

- label_text_button:

  (`string`)  
  label for the text input button.

- label_lock_button:

  (`string`)  
  label for the lock button.

- label_select_all_button:

  (`string`)  
  label for the selecting all genes button.

- label_select_none_button:

  (`string`)  
  label for the selecting no genes button.

- max_options:

  (`count`)  
  maximum number of gene options rendering and selected via "Select
  All".

- max_selected:

  (`count`)  
  maximum number of genes which can be selected.

## Value

The UI part.

## See also

[`geneSpecServer()`](https://insightsengineering.github.io/teal.modules.hermes/reference/geneSpecServer.md)
for the module server and a complete example.

## Examples

``` r
geneSpecInput("my_genes", list(mean = colMeans), label_funs = "Please select function")
#> <div style="display: flex; justify-content: space-around;">
#>   <label>Select Gene(s)</label>
#>   <a id="my_genes-select_none_button" href="#" class="action-button action-link">
#>     <span class="action-label">
#>       <bslib-tooltip placement="top" bsOptions="[]" width="10px" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>         <template>Select None</template>
#>         <span>
#>           <i aria-label="remove-circle icon" class="glyphicon glyphicon-remove-circle" role="presentation"></i>
#>         </span>
#>       </bslib-tooltip>
#>     </span>
#>   </a>
#>   <a id="my_genes-select_all_button" href="#" class="action-button action-link">
#>     <span class="action-label">
#>       <bslib-tooltip placement="top" bsOptions="[]" width="10px" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>         <template>Select All Genes (first 200)</template>
#>         <span>
#>           <i aria-label="ok-circle icon" class="glyphicon glyphicon-ok-circle" role="presentation"></i>
#>         </span>
#>       </bslib-tooltip>
#>     </span>
#>   </a>
#>   <a id="my_genes-text_button" href="#" class="action-button action-link">
#>     <span class="action-label">
#>       <bslib-tooltip placement="top" bsOptions="[]" width="10px" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>         <template>Enter list of genes</template>
#>         <span>
#>           <i class="fas fa-font" role="presentation" aria-label="font icon"></i>
#>         </span>
#>       </bslib-tooltip>
#>     </span>
#>   </a>
#>   <bslib-tooltip placement="top" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>     <template>Lock gene selection (so that it does not get updated when filtering)</template>
#>     <div class="form-group shiny-input-container" style="width:10px;">
#>       <div class="pretty p-toggle p-plain p-icon p-pulse">
#>         <input id="my_genes-lock_button" type="checkbox"/>
#>         <div class="state p-on">
#>           <i aria-label="lock icon" class="fas fa-lock icon" role="presentation"></i>
#>           <label>
#>             <span></span>
#>           </label>
#>         </div>
#>         <div class="state p-off">
#>           <i aria-label="lock-open icon" class="fas fa-lock-open icon" role="presentation"></i>
#>           <label>
#>             <span></span>
#>           </label>
#>         </div>
#>       </div>
#>     </div>
#>   </bslib-tooltip>
#> </div>
#> <div>
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label shiny-label-null" for="my_genes-genes" id="my_genes-genes-label"></label>
#>     <div>
#>       <select class="shiny-input-select form-control" id="my_genes-genes" multiple="multiple"><option value=""></option></select>
#>       <script type="application/json" data-for="my_genes-genes" data-eval="[&quot;render&quot;]">{"placeholder":"- Nothing selected -","render":"{\n          option: function(item, escape) {\n              return '<div style = \"padding: 2px 12px;\">' +\n                        '<span style=\"font-size: inherit;\">' + item.label +\n                        ' <span style=\"color: red; font-size: xx-small;\" >' + item.value +\n                      '<\/div>'\n            }\n          }","searchField":["value","label"],"maxOptions":200,"maxItems":200,"plugins":["selectize-plugin-a11y"]}</script>
#>     </div>
#>   </div>
#> </div>
#> <div class="shiny-panel-conditional" data-display-if="input.genes &amp;&amp; input.genes.length &gt; 1" data-ns-prefix="my_genes-">
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label" id="my_genes-fun_name-label" for="my_genes-fun_name">Please select function</label>
#>     <div>
#>       <select id="my_genes-fun_name" class="shiny-input-select"><option value="mean" selected>mean</option></select>
#>       <script type="application/json" data-for="my_genes-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>     </div>
#>   </div>
#> </div>
```
