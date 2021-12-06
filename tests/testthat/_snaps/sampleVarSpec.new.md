# sampleVarSpecInput creates expected HTML

    Code
      sampleVarSpecInput("my_sample_var", label_vars = "Select cool variable",
        label_levels_button = "Combine those levels")
    Message <simpleMessage>
      This Font Awesome icon ('font fa-object-ungroup') does not exist:
      * if providing a custom `html_dependency` these `name` checks can 
        be deactivated with `verify_fa = FALSE`
    Output
      <div class="row">
        <div class="col-sm-8">
          <label class="control-label">Select cool variable</label>
        </div>
        <div class="col-sm-4">
          <button class="btn btn-default action-button pull-right list-genes" id="my_sample_var-levels_button" title="Combine those levels" type="button">
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
          <label class="control-label shiny-label-null" for="my_sample_var-sample_var"></label>
          <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-max-options="1" data-show-subtext="true" data-live-search="false" id="my_sample_var-sample_var" class="selectpicker form-control" multiple="multiple"><option value=""></option></select>
        </div>
      </div>

