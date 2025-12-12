# Module Input for Sample Variable Specification

This defines the input for the sample variable specification.

## Usage

``` r
sampleVarSpecInput(
  inputId,
  label_vars = "Select sample variable",
  label_levels_button = "Combine factor levels"
)
```

## Arguments

- inputId:

  (`string`)  
  the ID used to call the module input.

- label_vars:

  (`string`)  
  label for the sample variable selection.

- label_levels_button:

  (`string`)  
  label for the levels combination button.

## Value

The UI part.

## See also

[`sampleVarSpecServer()`](https://insightsengineering.github.io/teal.modules.hermes/reference/sampleVarSpecServer.md)
for the module server and a complete example.

## Examples

``` r
sampleVarSpecInput("my_vars", label_vars = "Select faceting variable")
#> <div>
#>   <span>
#>     <bslib-tooltip placement="top" bsOptions="[]" width="10px" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>       <template>Combine factor levels</template>
#>       <a id="my_vars-levels_button" href="#" class="action-button action-link">
#>         <span class="action-label">
#>           <i class="fas fa-table" role="presentation" aria-label="table icon"></i>
#>         </span>
#>       </a>
#>     </bslib-tooltip>
#>     Select faceting variable
#>   </span>
#> </div>
#> <div>
#>   <div>
#>     <script>
#>         $(function() {
#>           $('#my_vars-sample_var').on('change', function(e) {
#>             var select_concat = $(this).val().length ? $(this).val().join(', ') : 'NULL';
#>             $('#my_vars-sample_var_selected_text').html(select_concat);
#>           })
#>         })</script>
#>     <div>
#>       <div id="my_vars-sample_var_input" style="display: block;">
#>         <div class="form-group shiny-input-container">
#>           <label class="control-label shiny-label-null" for="my_vars-sample_var" id="my_vars-sample_var-label"></label>
#>           <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-allow-clear="true" data-max-options="1" data-show-subtext="true" data-live-search="false" data-container="body" data-state-input="true" id="my_vars-sample_var" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value=""></option></select>
#>         </div>
#>       </div>
#>       <div id="my_vars-sample_var_fixed" style="display: none;">
#>         <label class="control-label"></label>
#>         <code id="my_vars-sample_var_selected_text">NULL</code>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
