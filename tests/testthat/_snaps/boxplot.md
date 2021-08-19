# ui_g_boxplot creates expected HTML

    Code
      ui_g_boxplot(id = "testid", datasets = datasets, mae_name = mae_name,
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
                    <select id="testid-experiment_name"><option value="hd1" selected>hd1</option>
      <option value="hd2">hd2</option>
      <option value="hd3">hd3</option></select>
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
                  <label class="control-label" id="testid-genes-label" for="testid-genes">Select gene(s) of interest</label>
                  <div>
                    <select id="testid-genes" class="form-control" multiple="multiple"><option value=""></option></select>
                    <script type="application/json" data-for="testid-genes">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="testid-x_var">Optional stratifying variable</label>
                  <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-x_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="testid-color_var">Optional color variable</label>
                  <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-color_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="testid-facet_var">Optional facet variable</label>
                  <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-facet_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                </div>
                <label>Jitter</label>
                <div class="form-group shiny-input-container">
                  <input id="testid-jitter" type="checkbox" class="sw-switchInput" data-input-id="testid-jitter" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini"/>
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

