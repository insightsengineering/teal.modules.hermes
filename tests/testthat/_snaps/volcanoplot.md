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
                <div class="row">
                  <div class="col-sm-8">
                    <label class="control-label">Compare Groups</label>
                  </div>
                  <div class="col-sm-4">
                    <button class="btn btn-default action-button pull-right list-genes" id="testid-compare_group-levels_button" title="Please group here into 2 levels" type="button">
                      <span>
                        <i class="fa fa-font fa-object-ungroup" role="presentation" aria-label="font fa-object-ungroup icon"></i>
                      </span>
                    </button>
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
      }</style>
                  </div>
                </div>
                <div class="custom-select-input">
                  <div class="form-group shiny-input-container">
                    <label class="control-label shiny-label-null" for="testid-compare_group-sample_var"></label>
                    <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="testid-compare_group-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
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

