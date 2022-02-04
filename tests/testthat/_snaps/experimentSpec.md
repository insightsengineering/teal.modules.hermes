# experimentSpecInput creates expected HTML

    Code
      experimentSpecInput("my_experiment", datasets = datasets, mae_name = mae_name,
        label_experiments = "Please select the best experiment")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="my_experiment-name-label" for="my_experiment-name">Please select the best experiment</label>
        <div>
          <select id="my_experiment-name"><option value="hd1" selected>hd1</option>
      <option value="hd2">hd2</option>
      <option value="hd3">hd3</option></select>
          <script type="application/json" data-for="my_experiment-name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>

