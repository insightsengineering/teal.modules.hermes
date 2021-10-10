# adtteSpecInput creates expected HTML

    Code
      adtteSpecInput("adtte", label_paramcd = "Select right PARAMCD")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="adtte-paramcd-label" for="adtte-paramcd">Select right PARAMCD</label>
        <div>
          <select id="adtte-paramcd"><option value="" selected></option></select>
          <script type="application/json" data-for="adtte-paramcd">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>

