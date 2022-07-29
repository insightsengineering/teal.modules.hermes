# sampleVarSpecInput creates expected HTML

    Code
      sampleVarSpecInput("my_sample_var", label_vars = "Select cool variable",
        label_levels_button = "Combine those levels")
    Output
      <div class="row">
        <div class="col-sm-8">
          <label class="control-label">Select cool variable</label>
        </div>
        <div class="col-sm-4">
          <button class="btn btn-default action-button pull-right list-genes" id="my_sample_var-levels_button" title="Combine those levels" type="button">
            <span>
              <i class="fas fa-table" role="presentation" aria-label="table icon"></i>
            </span>
          </button>
        </div>
      </div>
      <div class="custom-select-input">
        <div class="form-group shiny-input-container">
          <label class="control-label shiny-label-null" for="my_sample_var-sample_var"></label>
          <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="my_sample_var-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
        </div>
      </div>

