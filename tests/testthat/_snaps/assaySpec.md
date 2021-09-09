# assaySpecInput creates expected HTML

    Code
      assaySpecInput("my_assay", label_assays = "Please select the best assay")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="my_assay-name-label" for="my_assay-name">Please select the best assay</label>
        <div>
          <select id="my_assay-name"><option value="" selected></option></select>
          <script type="application/json" data-for="my_assay-name">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>

