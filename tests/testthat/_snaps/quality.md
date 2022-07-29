# ui_g_quality creates expected HTML

    Code
      ui_g_quality(id = "testid", datasets = datasets, mae_name = mae_name,
        pre_output = NULL, post_output = NULL)
    Output
      <div class="row">
        <div>
          <div class="col-md-3">
            <div class="well">
              <div>
                <div class="block mb-4 p-1">
                  <label class="text-primary block -ml-1">
                    <strong>Reporter</strong>
                  </label>
                  <div class="simple_reporter_container">
                    <button id="testid-simple_reporter-add_report_card_simple-add_report_card_button" type="button" class="simple_report_button btn btn-primary action-button" title="Add Card">
                      <span>
                        <i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>
                      </span>
                    </button>
                    <button id="testid-simple_reporter-download_button_simple-download_button" type="button" class="simple_report_button btn btn-primary action-button" title="Downlaod">
                      <span>
                        <i class="fa fa-download" role="presentation" aria-label="download icon"></i>
                      </span>
                    </button>
                    <button id="testid-simple_reporter-reset_button_simple-reset_reporter" type="button" class="simple_report_button btn btn-warning action-button" title="Reset">
                      <span>
                        <i class="fa fa-xmark" role="presentation" aria-label="xmark icon"></i>
                      </span>
                    </button>
                  </div>
                </div>
                <label class="text-primary">Encodings</label>
                <span class="help-block">
                  Analysis of MAE:
                  <code>MyMAE</code>
                </span>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-experiment-name-label" for="testid-experiment-name">Select Experiment</label>
                  <div>
                    <select id="testid-experiment-name"><option value="hd1" selected>hd1</option>
      <option value="hd2">hd2</option>
      <option value="hd3">hd3</option></select>
                    <script type="application/json" data-for="testid-experiment-name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-plot_type-label" for="testid-plot_type">Plot Type</label>
                  <div>
                    <select id="testid-plot_type"><option value="Histogram" selected>Histogram</option>
      <option value="Q-Q Plot">Q-Q Plot</option>
      <option value="Density">Density</option>
      <option value="Boxplot">Boxplot</option>
      <option value="Top Genes Plot">Top Genes Plot</option>
      <option value="Correlation Heatmap">Correlation Heatmap</option></select>
                    <script type="application/json" data-for="testid-plot_type" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div data-display-if="input.plot_type == &#39;Top Genes Plot&#39; || input.plot_type == &#39;Correlation Heatmap&#39;" data-ns-prefix="testid-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-assay-name-label" for="testid-assay-name">Select Assay</label>
                    <div>
                      <select id="testid-assay-name"><option value="" selected></option></select>
                      <script type="application/json" data-for="testid-assay-name">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <label class="text-primary">Gene Filter Settings</label>
                <div class="form-group shiny-input-container">
                  <input id="testid-filter_gene" type="checkbox" class="sw-switchInput" data-input-id="testid-filter_gene" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini" checked="checked"/>
                </div>
                <div data-display-if="input.filter_gene" data-ns-prefix="testid-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-min_cpm-label" for="testid-min_cpm">Minimum CPM</label>
                    <input class="js-range-slider" id="testid-min_cpm" data-skin="shiny" data-min="1" data-max="10" data-from="5" data-step="1" data-grid="true" data-grid-num="9" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                  </div>
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-min_cpm_prop-label" for="testid-min_cpm_prop">Minimum CPM Proportion</label>
                    <input class="js-range-slider" id="testid-min_cpm_prop" data-skin="shiny" data-min="0.01" data-max="0.99" data-from="0.25" data-step="0.01" data-grid="true" data-grid-num="9.8" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                  </div>
                  <div class="form-group shiny-input-container">
                    <label class="control-label" for="testid-annotate">Required Annotations</label>
                    <select data-actions-box="true" data-none-selected-text="- Nothing selected -" data-max-options="Inf" data-show-subtext="true" data-live-search="false" id="testid-annotate" class="selectpicker form-control" multiple="multiple"><option value="" selected></option></select>
                  </div>
                </div>
                <label class="text-primary">Sample Filter Settings</label>
                <div class="form-group shiny-input-container">
                  <input id="testid-filter_sample" type="checkbox" class="sw-switchInput" data-input-id="testid-filter_sample" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini" checked="checked"/>
                </div>
                <div data-display-if="input.filter_sample" data-ns-prefix="testid-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-min_corr-label" for="testid-min_corr">Minimum Correlation</label>
                    <input class="js-range-slider" id="testid-min_corr" data-skin="shiny" data-min="0.01" data-max="0.99" data-from="0.5" data-step="0.01" data-grid="true" data-grid-num="9.8" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                  </div>
                  <div id="testid-min_depth" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="testid-min_depth-label">
                    <label class="control-label" id="testid-min_depth-label" for="testid-min_depth">Minimum Depth</label>
                    <div class="shiny-options-group">
                      <div class="radio">
                        <label>
                          <input type="radio" name="testid-min_depth" value="Default" checked="checked"/>
                          <span>Default</span>
                        </label>
                      </div>
                      <div class="radio">
                        <label>
                          <input type="radio" name="testid-min_depth" value="Specify"/>
                          <span>Specify</span>
                        </label>
                      </div>
                    </div>
                  </div>
                  <div data-display-if="input.min_depth == &#39;Specify&#39;" data-ns-prefix="testid-">
                    <div class="form-group shiny-input-container">
                      <label class="control-label shiny-label-null" for="testid-min_depth_continuous" id="testid-min_depth_continuous-label"></label>
                      <input class="js-range-slider" id="testid-min_depth_continuous" data-skin="shiny" data-min="1" data-max="10" data-from="1" data-step="1" data-grid="true" data-grid-num="9" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div class="col-md-9">
            <div class="well">
              <div class="pre-output"></div>
              <div class="output">
                <div id="testid-plot" class="shiny-plot-output" style="width:100%;height:400px;"></div>
              </div>
              <div class="post-output"></div>
            </div>
          </div>
        </div>
      </div>

