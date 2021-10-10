# geneSpecInput creates expected HTML

    Code
      geneSpecInput("my_genes", funs = list(mean = colMeans), label_funs = "Please select function")
    Output
      <div class="row">
        <div class="col-sm-8">
          <label class="control-label">Select Gene(s)</label>
        </div>
        <div class="col-sm-4">
          <button class="btn btn-default action-button pull-right list-genes" id="my_genes-text_button" title="Enter list of genes" type="button">
            <span>
              <i class="fa fa-font fa-border" role="presentation" aria-label="font fa-border icon"></i>
            </span>
          </button>
          <div class="pull-right" title="Lock gene selection (so that it does not get updated when filtering)">
            <div class="form-group shiny-input-container">
              <div class="pretty p-toggle p-plain p-icon p-pulse">
                <input id="my_genes-lock_button" type="checkbox"/>
                <div class="state p-on">
                  <i class="icon fa fa-lock fa-border" role="presentation" aria-label="lock fa-border icon"></i>
                  <label>
                    <span></span>
                  </label>
                </div>
                <div class="state p-off">
                  <i class="icon fa fa-unlock-alt fa-border" role="presentation" aria-label="unlock-alt fa-border icon"></i>
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
          <label class="control-label shiny-label-null" for="my_genes-genes"></label>
          <select data-live-search="true" data-actions-box="true" data-none-selected-text="- Nothing selected -" data-max-options="Inf" data-show-subtext="true" id="my_genes-genes" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
        </div>
      </div>
      <div data-display-if="input.genes.length &gt; 1" data-ns-prefix="my_genes-">
        <div class="form-group shiny-input-container">
          <label class="control-label" id="my_genes-fun_name-label" for="my_genes-fun_name">Please select function</label>
          <div>
            <select id="my_genes-fun_name"><option value="mean" selected>mean</option></select>
            <script type="application/json" data-for="my_genes-fun_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
          </div>
        </div>
      </div>

