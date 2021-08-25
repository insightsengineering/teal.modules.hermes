# ui_g_volcanoplot creates expected HTML

    Code
      ui_g_volcanoplot(id = "testid", datasets = datasets, mae_name = mae_name,
        pre_output = NULL, post_output = NULL)
    Output
      <div class="row">
        <div>
          <div class="col-md-3">
            <div class="well">
              <div>
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
                  <label class="control-label" id="testid-compare_group-label" for="testid-compare_group">Compare Groups</label>
                  <div>
                    <select id="testid-compare_group"><option value="" selected></option></select>
                    <script type="application/json" data-for="testid-compare_group">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-method-label" for="testid-method">Method</label>
                  <div>
                    <select id="testid-method"><option value="voom" selected>voom</option>
      <option value="deseq2">deseq2</option></select>
                    <script type="application/json" data-for="testid-method" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                  </div>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-log2_fc_thresh-label" for="testid-log2_fc_thresh">Log2 fold change threshold</label>
                  <input class="js-range-slider" id="testid-log2_fc_thresh" data-skin="shiny" data-min="0.1" data-max="10" data-from="2.5" data-step="0.1" data-grid="true" data-grid-num="9.9" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                </div>
                <div class="form-group shiny-input-container">
                  <label class="control-label" id="testid-adj_p_val_thresh-label" for="testid-adj_p_val_thresh">Adjusted p-value threshold</label>
                  <input class="js-range-slider" id="testid-adj_p_val_thresh" data-skin="shiny" data-min="0.001" data-max="1" data-from="0.05" data-step="0.01" data-grid="true" data-grid-num="9.99" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
                </div>
                <label>Show Top Differentiated Genes</label>
                <div class="form-group shiny-input-container">
                  <input id="testid-show_top_gene" type="checkbox" class="sw-switchInput" data-input-id="testid-show_top_gene" data-on-text="ON" data-off-text="OFF" data-label-width="auto" data-handle-width="auto" data-size="mini"/>
                </div>
              </div>
            </div>
          </div>
          <div class="col-md-9">
            <div class="well">
              <div id="pre-output"></div>
              <div id="output">
                <div>
                  <div id="testid-plot" class="shiny-plot-output" style="width:100%;height:400px;"></div>
                  <div id="testid-table" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
                </div>
              </div>
              <div id="post-output"></div>
            </div>
          </div>
        </div>
      </div>

