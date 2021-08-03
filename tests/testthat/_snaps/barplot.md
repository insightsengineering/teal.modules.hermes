# ui_g_barplot creates expected HTML

    Code
      ui_g_barplot(id = "testid", datasets = datasets, mae_name = mae_name,
        pre_output = NULL, post_output = NULL)
    Output
      <div class="row">
        <div>
          <div class="col-md-3">
            <div class="well">
              <div>
                <label class="text-primary">Encodings</label>
                <span class="help-block">
                  Analysis of MAE:
                  <code>MyMAE</code>
                </span>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-experiment_name-label" for="testid-experiment_name">Select experiment</label>
                  <div>
                    <select id="testid-experiment_name"><option value="se1" selected>se1</option>
      <option value="se2">se2</option>
      <option value="se3">se3</option></select>
                    <script type="application/json" data-for="testid-experiment_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-assay_name-label" for="testid-assay_name">Select assay</label>
                  <div>
                    <select id="testid-assay_name"><option value="" selected></option></select>
                    <script type="application/json" data-for="testid-assay_name">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="testid-facet_var">Optional facet variable</label>
                  <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-facet_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="testid-fill_var">Optional fill variable</label>
                  <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-fill_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-x_var-label" for="testid-x_var">Select x gene</label>
                  <div>
                    <select id="testid-x_var" class="form-control"><option value="" selected></option></select>
                    <script type="application/json" data-for="testid-x_var">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-percentiles-label" for="testid-percentiles">Select quantiles to be displayed</label>
                  <input class="js-range-slider" id="testid-percentiles" data-skin="shiny" data-type="double" data-min="0" data-max="1" data-from="0.2" data-to="0.8" data-step="0.01" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
                </div>
              </div>
            </div>
          </div>
          <div class="col-md-9">
            <div class="well">
              <div id="pre-output"></div>
              <div id="output">
                <div id="testid-plot" class="shiny-plot-output" style="width:100%;height:400px;"></div>
              </div>
              <div id="post-output"></div>
            </div>
          </div>
        </div>
      </div>

