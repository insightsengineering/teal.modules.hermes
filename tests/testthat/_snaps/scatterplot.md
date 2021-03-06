# ui_g_scatterplot creates expected HTML

    Code
      ui_g_scatterplot(id = "testid", datasets = datasets, mae_name = mae_name,
        summary_funs = list(Mean = colMeans), pre_output = NULL, post_output = NULL)
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
                    <label class="control-label">Select x Gene(s)</label>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x_spec-select_none_button" title="Select None" type="button">
                      <span>
                        <i aria-label="remove-circle icon" class="glyphicon glyphicon-remove-circle" role="presentation"></i>
                      </span>
                    </button>
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x_spec-select_all_button" title="Select All Genes (first 200)" type="button">
                      <span>
                        <i aria-label="ok-circle icon" class="glyphicon glyphicon-ok-circle" role="presentation"></i>
                      </span>
                    </button>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-x_spec-text_button" title="Enter list of genes" type="button">
                      <span>
                        <i class="fa fa-fas fa-font" role="presentation" aria-label="fas fa-font icon"></i>
                      </span>
                    </button>
                    <div class="pull-right" title="Lock gene selection (so that it does not get updated when filtering)">
                      <div class="form-group shiny-input-container">
                        <div class="pretty p-toggle p-plain p-icon p-pulse">
                          <input id="testid-x_spec-lock_button" type="checkbox"/>
                          <div class="state p-on">
                            <i class="icon fa fa-fas fa-lock" role="presentation" aria-label="fas fa-lock icon"></i>
                            <label>
                              <span></span>
                            </label>
                          </div>
                          <div class="state p-off">
                            <i class="icon fa fa-fas fa-lock-open" role="presentation" aria-label="fas fa-lock-open icon"></i>
                            <label>
                              <span></span>
                            </label>
                          </div>
                        </div>
                      </div>
                    </div>
                    <style type="text/css">.pretty {
        margin-right: 0;
      }
      
      .list-genes {
        padding: 0;
        margin: 0 0 0 0.5rem;
        background: transparent !important;
        outline: none !important;
        box-shadow: none !important;
        border: 0;
      }
      
      .custom-select-input .bootstrap-select .dropdown-menu {
        min-width: fit-content;
      }
      
      .selectize-input {
        max-height: 102px;
        overflow-y: auto;
      }
      
      div.shiny-radiomatrix input[type="radio"] {
        display: none;
      }
      
      div.shiny-radiomatrix .radio-parent {
        display: block;
        position: relative;
        cursor: pointer;
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
        top: -10px;
        left: -13px;
      }
      
      /* Hide the browser's default radio button */
      div.shiny-radiomatrix .radio-parent input {
        position: absolute;
        opacity: 0;
        cursor: pointer;
        height: 0;
        width: 0;
      }
      
      /* Create a custom radio button */
      div.shiny-radiomatrix .checkmark {
        position: absolute;
        height: 25px;
        width: 25px;
        background-color: #eee;
        z-index: 25;
        transition: 0.5s all ease-in-out;
      }
      
      div.shiny-radiomatrix .radio-parent:hover input ~ .checkmark {
        background-color: #2195f34b;
      }
      
      div.shiny-radiomatrix .radio-parent input:checked ~ .checkmark {
        background-color: #2196f3;
      }
      
      div.shiny-radiomatrix table {
        border-spacing: 0;
        border-collapse: collapse;
        overflow: hidden;
      }
      
      div.shiny-radiomatrix td,
      div.shiny-radiomatrix th {
        padding: 10px;
        position: relative;
        transition: 0.5s all ease-in-out;
      }
      
      div.shiny-radiomatrix tr:hover {
        background-color: rgba(151, 151, 151, 0.1);
      }
      
      div.shiny-radiomatrix td:hover::before,
      div.shiny-radiomatrix th:hover::before {
        background-color: rgba(151, 151, 151, 0.1);
        content: "\00a0";
        height: 1000vh;
        left: 0;
        position: absolute;
        top: -500vh;
        width: 100%;
      }
      
      .dataTable-container table td{
        white-space: unset !important;
      }</style>
                  </div>
                </div>
                <div class="custom-select-input">
                  <div class="form-group shiny-input-container">
                    <label class="control-label shiny-label-null" for="testid-x_spec-genes" id="testid-x_spec-genes-label"></label>
                    <div>
                      <select id="testid-x_spec-genes" class="form-control" multiple="multiple"><option value=""></option></select>
                      <script type="application/json" data-for="testid-x_spec-genes" data-eval="[&quot;render&quot;]">{"render":"{\n          option: function(item, escape) {\n              return '<div> <div style = \"font-size: inherit; display: inline\">' + item.label + '<\/div>' +\n                ' <div style=\"color: #808080; font-size: xx-small; display: inline\" >' + item.value + '<\/div> <\/div>'\n            }\n          }","searchField":["value","label"],"maxOptions":200,"maxItems":200,"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div data-display-if="input.genes &amp;&amp; input.genes.length &gt; 1" data-ns-prefix="testid-x_spec-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-x_spec-fun_name-label" for="testid-x_spec-fun_name">Select Gene Summary</label>
                    <div>
                      <select id="testid-x_spec-fun_name"><option value="Mean" selected>Mean</option></select>
                      <script type="application/json" data-for="testid-x_spec-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div class="row">
                  <div class="col-sm-8">
                    <label class="control-label">Select y Gene(s)</label>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-y_spec-select_none_button" title="Select None" type="button">
                      <span>
                        <i aria-label="remove-circle icon" class="glyphicon glyphicon-remove-circle" role="presentation"></i>
                      </span>
                    </button>
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-y_spec-select_all_button" title="Select All Genes (first 200)" type="button">
                      <span>
                        <i aria-label="ok-circle icon" class="glyphicon glyphicon-ok-circle" role="presentation"></i>
                      </span>
                    </button>
                  </div>
                  <div class="col-sm-2">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-y_spec-text_button" title="Enter list of genes" type="button">
                      <span>
                        <i class="fa fa-fas fa-font" role="presentation" aria-label="fas fa-font icon"></i>
                      </span>
                    </button>
                    <div class="pull-right" title="Lock gene selection (so that it does not get updated when filtering)">
                      <div class="form-group shiny-input-container">
                        <div class="pretty p-toggle p-plain p-icon p-pulse">
                          <input id="testid-y_spec-lock_button" type="checkbox"/>
                          <div class="state p-on">
                            <i class="icon fa fa-fas fa-lock" role="presentation" aria-label="fas fa-lock icon"></i>
                            <label>
                              <span></span>
                            </label>
                          </div>
                          <div class="state p-off">
                            <i class="icon fa fa-fas fa-lock-open" role="presentation" aria-label="fas fa-lock-open icon"></i>
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
                    <label class="control-label shiny-label-null" for="testid-y_spec-genes" id="testid-y_spec-genes-label"></label>
                    <div>
                      <select id="testid-y_spec-genes" class="form-control" multiple="multiple"><option value=""></option></select>
                      <script type="application/json" data-for="testid-y_spec-genes" data-eval="[&quot;render&quot;]">{"render":"{\n          option: function(item, escape) {\n              return '<div> <div style = \"font-size: inherit; display: inline\">' + item.label + '<\/div>' +\n                ' <div style=\"color: #808080; font-size: xx-small; display: inline\" >' + item.value + '<\/div> <\/div>'\n            }\n          }","searchField":["value","label"],"maxOptions":200,"maxItems":200,"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div data-display-if="input.genes &amp;&amp; input.genes.length &gt; 1" data-ns-prefix="testid-y_spec-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-y_spec-fun_name-label" for="testid-y_spec-fun_name">Select Gene Summary</label>
                    <div>
                      <select id="testid-y_spec-fun_name"><option value="Mean" selected>Mean</option></select>
                      <script type="application/json" data-for="testid-y_spec-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                </div>
                <div class="panel-group">
                  <style type="text/css">.panel-title { font-size: 14px; } /*same as everywhere else*/
      .panel-body { background-color: #f5f5f5; } /*same as panel-title*/</style>
                  <input id="settings_item" type="checkbox" value="TRUE" class="shinyjs-hide"/>
                  <div class="panel panel-default">
                    <div id="settings_item_div" class="panel-heading collapsed" data-toggle="collapse" href="#settings_item_panel_body_2463" aria-expanded="false">
                      <i class="fa fa-angle-down dropdown-icon" role="presentation" aria-label="angle-down icon"></i>
                      <label class="panel-title" style="display:inline">Additional Settings</label>
                    </div>
                    <div class="panel-collapse collapse " id="settings_item_panel_body_2463">
                      <div class="panel-body">
                        <div class="row">
                          <div class="col-sm-8">
                            <label class="control-label">Optional color variable</label>
                          </div>
                          <div class="col-sm-4">
                            <button class="btn btn-default action-button pull-right list-genes" id="testid-color_var-levels_button" title="Combine factor levels" type="button">
                              <span>
                                <i class="fa fa-fas fa-table" role="presentation" aria-label="fas fa-table icon"></i>
                              </span>
                            </button>
                          </div>
                        </div>
                        <div class="custom-select-input">
                          <div class="form-group shiny-input-container">
                            <label class="control-label shiny-label-null" for="testid-color_var-sample_var"></label>
                            <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-color_var-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                          </div>
                        </div>
                        <div class="row">
                          <div class="col-sm-8">
                            <label class="control-label">Optional facet variable</label>
                          </div>
                          <div class="col-sm-4">
                            <button class="btn btn-default action-button pull-right list-genes" id="testid-facet_var-levels_button" title="Combine factor levels" type="button">
                              <span>
                                <i class="fa fa-fas fa-table" role="presentation" aria-label="fas fa-table icon"></i>
                              </span>
                            </button>
                          </div>
                        </div>
                        <div class="custom-select-input">
                          <div class="form-group shiny-input-container">
                            <label class="control-label shiny-label-null" for="testid-facet_var-sample_var"></label>
                            <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-facet_var-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                          </div>
                        </div>
                        <div class="form-group shiny-input-container">
                          <label class="control-label" id="testid-smooth_method-label" for="testid-smooth_method">Select smoother</label>
                          <div>
                            <select id="testid-smooth_method"><option value="lm" selected>Linear</option>
      <option value="loess">Loess</option>
      <option value="none">None</option></select>
                            <script type="application/json" data-for="testid-smooth_method" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
              <div id="pre-output"></div>
              <div id="output">
                <div id="testid-plot" class="shiny-plot-output" style="width:100%;height:400px;"></div>
              </div>
              <div id="post-output"></div>
            </div>
          </div>
        </div>
      </div>

