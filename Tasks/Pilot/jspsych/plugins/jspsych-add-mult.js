/**
* Metaexploration pilot task plugin
**/


jsPsych.plugins["add-mult"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'add-mult',
    description: '',
    parameters: {
      choices: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        array: true,
        pretty_name: 'Choices',
        default: jsPsych.ALL_KEYS,
        description: 'The keys the subject is allowed to press to respond to the stimulus.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed below the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: 2000,
        description: 'How long to show trial before it ends.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    var new_html = '<div id="jspsych-html-keyboard-response-stimulus">'+trial.stimulus+'</div>';
    var pause = 0

    var trial_index = jsPsych.data.get().filter({trial_type: 'add-mult'}).count()
    var block = Math.floor(trial_index/trialsperblock)

    var setTimeoutHandlers = []

    var keyboardListener = new Object

    var response = {rt: -1, key: -1}
    var correct_answer = []
    var calculation = []

    var both_choices = [["F","J"],["space"]]
    var choices = new Array

    var time_home = 0
    var math_rt = 0
    var timeout_phase = 0
    var problem_type = []
    var stim_1 = []
    var stim_2 = []
    var accuracy = -1


    // function to end trial when it is time
    var end_trial = function(){

      kill_listeners()
      kill_timers()

      // gather the data to store for the trial
      var trial_data = {
        "subid": subid,
        "calculation":calculation,
        "correct_calc":correct_answer,
        "type": problem_type,
        "stim_1": stim_1,
        "stim_2":stim_2,
        "accuracy":accuracy,
        "math_rt": math_time,
        "trial_nr": trial_index,
      }
      jsPsych.finishTrial(trial_data)
    }

    // function to handle responses by the subject
    var after_response = function(info){

      kill_listeners()
      kill_timers()

      if (pause == 0) {

        if (response.key == -1){
          response = info
        }

        cent_stim.innerHTML += '<div class = "centered">'+trial.stimulus_one+' + '+trial.stimulus_two+'</div>'

        var handle_response = jsPsych.pluginAPI.setTimeout(function() {
          kill_listeners()
          end_trial()
        }, trial.trial_duration)
      } else {
        pause = 0
        next_trial()
      }
    }

    var display_stimuli = function(){

      display_element.innerHTML = '<div id="jspsych-content"></div>'
      content = display_element.querySelector('#jspsych-content')

      content.innerHTML += '<div id= "jspsych-box0"></div>'
      content.innerHTML += '<div id= "jspsych-box1"></div>'
      content.innerHTML += '<div id= "jspsych-box2"></div>'
      content.innerHTML += '<div id= "jspsych-box0"></div>'
      content.innerHTML += '<div id="break"></div>'

      content.innerHTML += '<div id= "jspsych-center-left"></div>'
      content.innerHTML += '<div id= "jspsych-center-stim"></div>'
      content.innerHTML += '<div id= "jspsych-center-right"></div>'
      content.innerHTML += '<div id="break"></div>'
      content.innerHTML += '<div id= "jspsych-bottom"></div>'

      cent_l = content.querySelector('#jspsych-center-left')
      cent_stim = content.querySelector('#jspsych-center-stim')
      cent_r = content.querySelector('#jspsych-center-right')
      bottom = content.querySelector('#jspsych-bottom')

      cent_l.innerHTML += '<div id="jspsych-box3"></div><div id="jspsych-box5"></div>'
      cent_r.innerHTML += '<div id="jspsych-box4"></div><div id="jspsych-box6"></div>'

      box_l = content.querySelector('#jspsych-box3')
      box_l.style.backgroundPosition = "center"
      box_l.style.backgroundRepeat = "no-repeat"

      box_r = content.querySelector('#jspsych-box6')
      box_r.style.backgroundPosition = "center"
      box_r.style.backgroundRepeat = "no-repeat"
    }

    var start_response_listener = function(){
      if (pause == 0) {
        choices = []
      } else {
        choices = both_choices[1]
      }

      if(JSON.stringify(choices) != JSON.stringify(["none"])) {
        var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
          callback_function: after_response,
          valid_responses: choices,
          rt_method: 'date',
          persist: false,
          allow_held_key: false,
        })
      }
    }

    // kill timers and response listeners
    var kill_timers = function(){
      for (var i = 0; i < setTimeoutHandlers.length; i++) {
        clearTimeout(setTimeoutHandlers[i])
      }
    }
    var kill_listeners = function(){
      if(keyboardListener !== 'undefined'){
        jsPsych.pluginAPI.cancelAllKeyboardResponses()
      }
    }

    var next_trial = function(){

      var startTime = new Date()

      kill_timers()
      kill_listeners()

      display_stimuli()

      stim_1 = trial.stimulus_one
      stim_2 = trial.stimulus_two

      if (trial.condition == '+'){
        correct_answer = trial.stimulus_one+trial.stimulus_two
        problem_type = 'add'
      }else{
        correct_answer = trial.stimulus_one*trial.stimulus_two
        problem_type = 'mult'
      }

      cent_stim.innerHTML += '<div class = "centered">'+trial.stimulus_one+' '+trial.condition+' '+trial.stimulus_two+'</div>'
      bottom.innerHTML+= '<input type="number" min="0" id = "count_report" onkeydown="return event.keyCode !== 69 && event.keyCode !== 189" value="" autofocus="autofocus">'
      bottom.innerHTML += '<br><button id = "bttn">Submit</button>'

      var input = document.getElementById("count_report")
      input.addEventListener("keyup", function(event) {
        if (event.keyCode === 13) {
          event.preventDefault();
          document.getElementById("bttn").click()
        }
      })
      input.focus()
      document.getElementById("bttn").onclick = function() {input_store()}
      function input_store() {
        calculation = document.getElementById("count_report").value;
        if (calculation == correct_answer){
          accuracy = 1
        } else{
          accuracy = 0
        }

        var endTime = new Date()
        math_time = endTime - startTime
        end_trial()
      }
      start_response_listener()
    }

    if ((trial_index==0)||(trial_index%trialsperblock!= 0)){
      next_trial()
    } else {
      pause = 1
      display_element.innerHTML = "You have completed "+trial_index+"/"+nrtrials+" trials. Press spacebar to continue."
      start_response_listener()
    }
  }

  return plugin
})()
