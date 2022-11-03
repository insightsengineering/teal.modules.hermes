# geneSpecInput creates expected HTML

    Code
      geneSpecInput("my_genes", funs = list(mean = colMeans), label_funs = "Please select function")
    Output
      <div class="row">
        <div class="col-sm-8">
          <label class="control-label">Select Gene(s)</label>
        </div>
        <div class="col-sm-2">
          <button class="btn btn-default action-button pull-right list-genes" id="my_genes-select_none_button" title="Select None" type="button">
            <span>
              <i aria-label="remove-circle icon" class="glyphicon glyphicon-remove-circle" role="presentation"></i>
            </span>
          </button>
          <button class="btn btn-default action-button pull-right list-genes" id="my_genes-select_all_button" title="Select All Genes (first 200)" type="button">
            <span>
              <i aria-label="ok-circle icon" class="glyphicon glyphicon-ok-circle" role="presentation"></i>
            </span>
          </button>
        </div>
        <div class="col-sm-2">
          <button class="btn btn-default action-button pull-right list-genes" id="my_genes-text_button" title="Enter list of genes" type="button">
            <span>
              <i class="fas fa-font" role="presentation" aria-label="font icon"></i>
            </span>
          </button>
          <div class="pull-right" title="Lock gene selection (so that it does not get updated when filtering)">
            <div class="form-group shiny-input-container">
              <div class="pretty p-toggle p-plain p-icon p-pulse">
                <input id="my_genes-lock_button" type="checkbox"/>
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
          <label class="control-label shiny-label-null" for="my_genes-genes" id="my_genes-genes-label"></label>
          <div>
            <select id="my_genes-genes" class="form-control" multiple="multiple"><option value=""></option></select>
            <script type="application/json" data-for="my_genes-genes" data-eval="[&quot;render&quot;]">{"render":"{\n          option: function(item, escape) {\n              return '<div> <span style=\"font-size: inherit;\">' + item.label + '<\/div>' +\n                ' <span style=\"color: #808080; font-size: xx-small;\" >' + item.value + '<\/div> <\/div>'\n            }\n          }","searchField":["value","label"],"maxOptions":200,"maxItems":200,"plugins":["selectize-plugin-a11y"]}</script>
          </div>
        </div>
      </div>
      <div data-display-if="input.genes &amp;&amp; input.genes.length &gt; 1" data-ns-prefix="my_genes-">
        <div class="form-group shiny-input-container">
          <label class="control-label" id="my_genes-fun_name-label" for="my_genes-fun_name">Please select function</label>
          <div>
            <select id="my_genes-fun_name"><option value="mean" selected>mean</option></select>
            <script type="application/json" data-for="my_genes-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
          </div>
        </div>
      </div>

