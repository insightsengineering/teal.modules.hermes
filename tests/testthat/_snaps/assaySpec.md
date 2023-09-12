# assaySpecInput creates expected HTML

    Code
      assaySpecInput("my_assay", label_assays = "Please select the best assay")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="my_assay-name-label" for="my_assay-name">Please select the best assay</label>
        <div>
          <select id="my_assay-name" class="form-control"></select>
          <script type="application/json" data-for="my_assay-name">{"placeholder":"- Nothing selected -","plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      <script>// Toggle enable/disable of a dropdown
      //
      // This can be done by `{shinyjs::enable/disable}` R package if that package is
      // added to the dependecies.
      // Parameters
      //  `message` should have `input_id` string and `disabled` logical properties.
      Shiny.addCustomMessageHandler('toggle_dropdown', function(message) {
        const input_id = message.input_id;
        const disabled = message.disabled;
      
        let el = document.getElementById(input_id)
      
        if (el.selectize !== undefined) {
          el = el.selectize;
          if (disabled) {
            el.lock();
            el.disable();
          } else {
            el.unlock();
            el.enable();
          }
        } else {
          // Fallback in case selectize is not enabled
          el.disabled = disabled ? 'disabled' : '';
        }
      });</script>

