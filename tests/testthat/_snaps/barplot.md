# ui_g_barplot creates expected HTML

    Code
      ui_g_barplot(id = "testid", datasets = datasets, mae_name = mae_name,
        summary_funs = list(Mean = colMeans), pre_output = NULL, post_output = NULL)
    Output
      <div class="row">
        <div>
          <div class="col-md-3">
            <div class="well">
              <div>
                <div>
                  <button class="add_card--hover btn btn-primary action-button" id="testid-addReportCard-add_report_card_button" type="button">
                    <span class="add_card--after">
                      <i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>
                    </span>
                  </button>
                  <button class="download_report--hover btn btn-primary action-button" id="testid-downloadButton-download_button" type="button">
                    <span class="download_report--after">
                      <i class="fa fa-download" role="presentation" aria-label="download icon"></i>
                    </span>
                  </button>
                  <button class="reset_report--hover btn btn-warning action-button" id="testid-resetButton-reset_reporter" type="button">
                    <span class="reset_report--after">
                      <i class="fa fa-xmark" role="presentation" aria-label="xmark icon"></i>
                    </span>
                  </button>
                </div>
                <br/>
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
                  <label class="control-label" id="testid-assay-name-label" for="testid-assay-name">Select Assay</label>
                  <div>
                    <select id="testid-assay-name"><option value="" selected></option></select>
                    <script type="application/json" data-for="testid-assay-name">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="row">
                  <div class="col-sm-8">
                    <label class="control-label">Select Facet Variable</label>
                  </div>
                  <div class="col-sm-4">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-facet-levels_button" title="Combine factor levels" type="button">
                      <span>
                        <i class="fas fa-table" role="presentation" aria-label="table icon"></i>
                      </span>
                    </button>
                  </div>
                </div>
                <div class="custom-select-input">
                  <div class="form-group shiny-input-container">
                    <label class="control-label shiny-label-null" for="testid-facet-sample_var"></label>
                    <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-facet-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                  </div>
                </div>
                <div class="row">
                  <div class="col-sm-8">
                    <label class="control-label">Select Gene(s)</label>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x-select_none_button" title="Select None" type="button">
                      <span>
                        <i aria-label="remove-circle icon" class="glyphicon glyphicon-remove-circle" role="presentation"></i>
                      </span>
                    </button>
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x-select_all_button" title="Select All Genes (first 200)" type="button">
                      <span>
                        <i aria-label="ok-circle icon" class="glyphicon glyphicon-ok-circle" role="presentation"></i>
                      </span>
                    </button>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x-text_button" title="Enter list of genes" type="button">
                      <span>
                        <i class="fas fa-font" role="presentation" aria-label="font icon"></i>
                      </span>
                    </button>
                    <div class="pull-right" title="Lock gene selection (so that it does not get updated when filtering)">
                      <div class="form-group shiny-input-container">
                        <div class="pretty p-toggle p-plain p-icon p-pulse">
                          <input id="testid-x-lock_button" type="checkbox"/>
                          <div class="state p-on">
                            <i class="icon fas fa-lock" role="presentation" aria-label="lock icon"></i>
                            <label>
                              <span></span>
                            </label>
                          </div>
                          <div class="state p-off">
                            <i class="icon fas fa-lock-open" role="presentation" aria-label="lock-open icon"></i>
                            <label>
                              <span></span>
                            </label>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="custom-select-input">
                  <div class="form-group shiny-input-container">
                    <label class="control-label shiny-label-null" for="testid-x-genes" id="testid-x-genes-label"></label>
                    <div>
                      <select id="testid-x-genes" class="form-control" multiple="multiple"><option value=""></option></select>
                      <script type="application/json" data-for="testid-x-genes" data-eval="[&quot;render&quot;]">{"render":"{\n          option: function(item, escape) {\n              return '<div> <span style = \"font-size: inherit;\">' + item.label + '<\/div>' +\n                ' <span style=\"color: #808080; font-size: xx-small;\" >' + item.value + '<\/div> <\/div>'\n            }\n          }","searchField":["value","label"],"maxOptions":200,"maxItems":200,"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div data-display-if="input.genes &amp;&amp; input.genes.length &gt; 1" data-ns-prefix="testid-x-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-x-fun_name-label" for="testid-x-fun_name">Select Gene Summary</label>
                    <div>
                      <select id="testid-x-fun_name"><option value="Mean" selected>Mean</option></select>
                      <script type="application/json" data-for="testid-x-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-percentiles-label" for="testid-percentiles">Select Quantiles</label>
                  <input class="js-range-slider" id="testid-percentiles" data-skin="shiny" data-type="double" data-min="0" data-max="1" data-from="0.2" data-to="0.8" data-step="0.01" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
                </div>
                <div class="panel-group">
                  <input id="settings_item" type="checkbox" value="TRUE" class="shinyjs-hide"/>
                  <div class="panel panel-default">
                    <div id="settings_item_div" class="panel-heading collapsed" data-toggle="collapse" href="#settings_item_panel_body_9115" aria-expanded="false">
                      <i class="fa fa-angle-down dropdown-icon" role="presentation" aria-label="angle-down icon"></i>
                      <label class="panel-title inline">Additional Settings</label>
                    </div>
                    <div class="panel-collapse collapse " id="settings_item_panel_body_9115">
                      <div class="panel-body">
                        <div class="row">
                          <div class="col-sm-8">
                            <label class="control-label">Optional Fill Variable</label>
                          </div>
                          <div class="col-sm-4">
                            <button class="btn btn-default action-button pull-right list-genes" id="testid-fill-levels_button" title="Combine factor levels" type="button">
                              <span>
                                <i class="fas fa-table" role="presentation" aria-label="table icon"></i>
                              </span>
                            </button>
                          </div>
                        </div>
                        <div class="custom-select-input">
                          <div class="form-group shiny-input-container">
                            <label class="control-label shiny-label-null" for="testid-fill-sample_var"></label>
                            <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-fill-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                          </div>
                        </div>
                      </div>
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

