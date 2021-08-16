# ui_g_pca creates expected HTML

    Code
      ui_g_pca(id = "testid", datasets = datasets, mae_name = mae_name, pre_output = NULL,
        post_output = NULL)
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
                <div data-display-if="input.tab_selected == &#39;PCA&#39;" data-ns-prefix="testid-">
                  <div class="form-group shiny-input-container">
                    <label class="control-label" for="testid-color_var">Optional color variable</label>
                    <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-color_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
                  </div>
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-x_var-label" for="testid-x_var">Select X-axis PC</label>
                    <div>
                      <select id="testid-x_var" class="form-control"><option value="" selected></option></select>
                      <script type="application/json" data-for="testid-x_var">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                  <div class="form-group shiny-input-container">
                    <label class="control-label" id="testid-y_var-label" for="testid-y_var">Select Y-axis PC</label>
                    <div>
                      <select id="testid-y_var" class="form-control"><option value="" selected></option></select>
                      <script type="application/json" data-for="testid-y_var">{"plugins":["selectize-plugin-a11y"]}</script>
                    </div>
                  </div>
                  <label>Show Variance %</label>
                  <div class="form-group shiny-input-container">
                    <input id="testid-var_pct" type="checkbox" class="sw-switchInput" data-input-id="testid-var_pct" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini" checked="checked"/>
                  </div>
                  <label>Show Label</label>
                  <div class="form-group shiny-input-container">
                    <input id="testid-label" type="checkbox" class="sw-switchInput" data-input-id="testid-label" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini" checked="checked"/>
                  </div>
                </div>
                <div data-display-if="input.tab_selected == &#39;PC and Sample Correlation&#39;" data-ns-prefix="testid-">
                  <label>Cluster columns</label>
                  <div class="form-group shiny-input-container">
                    <input id="testid-cluster_columns" type="checkbox" class="sw-switchInput" data-input-id="testid-cluster_columns" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini"/>
                  </div>
                </div>
                <label>View Matrix</label>
                <div class="form-group shiny-input-container">
                  <input id="testid-show_matrix" type="checkbox" class="sw-switchInput" data-input-id="testid-show_matrix" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini" checked="checked"/>
                </div>
              </div>
            </div>
          </div>
          <div class="col-md-9">
            <div class="well">
              <div id="pre-output"></div>
              <div id="output">
                <div class="tabbable">
                  <ul class="nav nav-tabs shiny-tab-input" id="testid-tab_selected" data-tabsetid="2004">
                    <li class="active">
                      <a href="#tab-2004-1" data-toggle="tab" data-value="PCA">PCA</a>
                    </li>
                    <li>
                      <a href="#tab-2004-2" data-toggle="tab" data-value="PC and Sample Correlation">PC and Sample Correlation</a>
                    </li>
                  </ul>
                  <div class="tab-content" data-tabsetid="2004">
                    <div class="tab-pane active" data-value="PCA" id="tab-2004-1">
                      <div class="col-sm-12">
                        <div style="height:20px;"></div>
                        <div id="testid-plot_pca" class="shiny-plot-output" style="width:100%;height:400px;"></div>
                        <br/>
                        <br/>
                        <br/>
                        <div id="testid-table_pca" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
                      </div>
                    </div>
                    <div class="tab-pane" data-value="PC and Sample Correlation" id="tab-2004-2">
                      <div class="col-sm-12">
                        <div style="height:20px;"></div>
                        <div id="testid-plot_cor" class="shiny-plot-output" style="width:100%;height:400px;"></div>
                        <br/>
                        <br/>
                        <br/>
                        <div id="testid-table_cor" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
              <div id="post-output"></div>
            </div>
          </div>
        </div>
      </div>

