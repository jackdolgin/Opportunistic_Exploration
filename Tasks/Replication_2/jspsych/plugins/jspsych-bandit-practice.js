jsPsych.plugins["bandit-practice"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'bandit-practice',
    description: '',
    parameters: {
      practice_block:{
        type: jsPsych.plugins.parameterType.INT,
        default: 0,
        description: ''
      },
      trial_nr: {
        type: jsPsych.plugins.parameterType.INT,
        default: 0,
        description: ''
      },
      horizon: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Choices',
        default: [],
        description: ''
      },
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
      rews:{
        type:jsPsych.plugins.parameterType.INT,
        default: [],
        description: 'Total reward.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: 60000,
        description: 'How long to show trial before it ends.'
      },
      feedback_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: 1000,
        description: 'How long to show trial before it ends.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    var pause = 0

    var setTimeoutHandlers = []

    var keyboardListener = new Object

    var response = {rt: -1, key: -1}
    var correct_answer = []
    var calculation =-1

    var all_choices = [["space"],[8,48,49,50,51,52,53,54,55,56,57,32,189]]
    var choices = new Array

    var math_rt = 0
    var problem_type =-1
    var stim_1 = []
    var stim_2 = []
    var accuracy = -1
    var b_color = []

    var part = -1
    var stage = -1
    var points_guess = '?'
    var add_sltn = []
    var mult_sltn = []
    var correct = -1
    var add_bground = []
    var mult_bground = []
    var opaque = 0

    var minus = 0
    var stims = []
    var add = side[0]
    var mult = side[1]
    var nr = 'XX' //no reward
    var keypress = []

    function gen_reward(min, max) {
      return Math.random() * (max - min) + min;
    }
    var reward = gen_reward(0,100)


    var provide_answer = function() {
      display_stimuli(4)
      start_response_listener()
    }

    var left_lever = jsPsych.randomization.sampleWithoutReplacement([1,3],1)
    var right_lever = jsPsych.randomization.sampleWithoutReplacement([2,4],1)


    // function to end trial when it is time
    var end_trial = function(){
      current_trial = current_trial+1

      kill_listeners()
      kill_timers()

      // gather the data to store for the trial
      var trial_data = {
        "subid": subid,
        "horizon":trial.horizon,
        "calculation":calculation,
        "type": problem_type,
        "stim_1": trial.stimulus_one,
        "stim_2":trial.stimulus_two,
        "accuracy":correct,
        "block": trial.practice_block,
        "trial_nr": current_trial,
      }

      var handle_totalscore = setTimeout(function() {
        var handle_iti = jsPsych.pluginAPI.setTimeout(function() {
          jsPsych.finishTrial(trial_data)
        }, trial.iti)
        setTimeoutHandlers.push(handle_iti)
      }, trial.totalscore_time)
      setTimeoutHandlers.push(handle_totalscore)
    }

    // function to handle responses by the subject
    var after_response = function(info){
      kill_listeners()
      kill_timers()

      part = part + 1
      keypress = info.key
      if (trial.practice_block == 2 && current_trial<4){
        display_stimuli(5)
      }else if(trial.practice_block == 3){

        if (String.fromCharCode(info.key)==' ' && points_guess!='?'){
          kill_listeners()
        }

        if (String.fromCharCode(info.key)!=' ') {

          if (info.key==8){
            points_guess ='?'
          } else {
            if (points_guess =='?') {
              points_guess = String.fromCharCode(info.key)
            } else {
              if (points_guess.length==(3)){
                points_guess = String.fromCharCode(info.key)
              } else if(points_guess.length==(2)){
                points_guess = points_guess+String.fromCharCode(info.key)
              }else{
                points_guess = points_guess[0] + String.fromCharCode(info.key)

                if (points_guess[0] == '0') {
                  points_guess = points_guess[1]
                }
              }
            }
          }
        }

        display_stimuli(4)
        provide_answer()
      }else{
        if (keypress == 70){
          current_side = content.querySelector('#jspsych-box'+current_trial+'-l')
        }else{
          current_side = content.querySelector('#jspsych-box'+current_trial+'-r')

        }
        display_stimuli(5)
      }

      if (trial.practice_block == 3){
        if ((String.fromCharCode(info.key)== ' '&& points_guess!='?')){
          kill_listeners()
          calculation = (Number(points_guess))
          if ((Number(points_guess))==(correct_answer)){
            correct = 1
          }else{
            correct = 0
          }
          display_stimuli(6)
          var handle_response = jsPsych.pluginAPI.setTimeout(function() {
            end_trial()
          }, trial.feedback_duration)
        }
      }

      update_trial = function(){
        for (var i=0; i<=current_trial; i++){

          default_l[i] = '<div style=background-color:'+selected_color_l+';color:black;">'+trial_hist_l[i]+'</div>'
          default_r[i] = '<div style=background-color:'+selected_color_r+';color:black;">'+trial_hist_r[i]+'</div>'
        }

      }

      update_trial()

      var handle_response = jsPsych.pluginAPI.setTimeout(function() {
        if (trial.practice_block != 3){
          end_trial()
        }

      }, trial.feedback_duration)

    }

    var display_stimuli = function(stage){

      kill_timers()
      kill_listeners()
      if (stage ==1){

        display_element.innerHTML = '<div id="jspsych-content"></div>'
        content = display_element.querySelector('#jspsych-content')

        content.innerHTML += '<div id= "jspsych-left"></div>'
        content.innerHTML += '<div id= "jspsych-center-left"></div>'
        content.innerHTML += '<div id= "jspsych-center-stim"></div>'
        content.innerHTML += '<div id= "jspsych-center-right"></div>'
        content.innerHTML += '<div id= "jspsych-right"></div>'

        content.innerHTML += '<div id="break"></div>'
        content.innerHTML += '<div id= "jspsych-bottom"></div>'

        cent_l = content.querySelector('#jspsych-center-left')
        cent_stim = content.querySelector('#jspsych-center-stim')
        cent_r = content.querySelector('#jspsych-center-right')
        add_side = content.querySelector('#jspsych-center-'+add)
        mult_side = content.querySelector('#jspsych-center-'+mult)


        add_side.innerHTML ='<div class = "problem-left" style = opacity:'+opaque+';><img src = "img/add.png" width="110" height="70"></div'
        mult_side.innerHTML = '<div class = "problem-left" style = opacity:'+opaque+';><img src = "img/mult.png" width="110" height="70"></div'

        bottom = content.querySelector('#jspsych-bottom')

        cent_l.innerHTML += '<div id="jspsych-box0-l"></div><div id="jspsych-box1-l"></div><div id="jspsych-box2-l"></div><div id="jspsych-box3-l"></div><div id="jspsych-box4-l">'

        cent_stim.innerHTML += '<div id="jspsych-center-box9"></div><div id="jspsych-center-box0"></div><div id="jspsych-center-box1"></div><div id="jspsych-center-box2"></div><div id="jspsych-center-box3"></div><div id="jspsych-center-box4"></div>'
        cent_r.innerHTML += '<div id="jspsych-box0-r"></div><div id="jspsych-box1-r"></div><div id="jspsych-box2-r"></div><div id="jspsych-box3-r"></div><div id="jspsych-box4-r"></div>'

        if (trial.horizon==long_horizon){
          cent_l.innerHTML +=  '<div id="jspsych-box5-l"></div><div id="jspsych-box6-l"></div><div id="jspsych-box7-l"></div>'
          cent_r.innerHTML +=  '<div id="jspsych-box5-r"></div><div id="jspsych-box6-r"></div><div id="jspsych-box7-r"></div>'
          cent_stim.innerHTML += '<div id="jspsych-center-box5"></div><div id="jspsych-center-box6"></div><div id="jspsych-center-box7"></div><div id="jspsych-center-box8"></div>'
        }

        box_0 = content.querySelector('#jspsych-box0-l')
        box_1 = content.querySelector('#jspsych-box1-l')
        box_2 = content.querySelector('#jspsych-box2-l')
        box_3 = content.querySelector('#jspsych-box3-l')
        box_4 = content.querySelector('#jspsych-box4-l')
        box_5 = content.querySelector('#jspsych-box5-l')
        box_6 = content.querySelector('#jspsych-box6-l')
        box_7 = content.querySelector('#jspsych-box7-l')

        box_0_r = content.querySelector('#jspsych-box0-r')
        box_1_r = content.querySelector('#jspsych-box1-r')
        box_2_r = content.querySelector('#jspsych-box2-r')
        box_3_r = content.querySelector('#jspsych-box3-r')
        box_4_r = content.querySelector('#jspsych-box4-r')
        box_5_r = content.querySelector('#jspsych-box5-r')
        box_6_r = content.querySelector('#jspsych-box6-r')
        box_7_r = content.querySelector('#jspsych-box7-r')

        center_box_9 = content.querySelector('#jspsych-center-box9')
        center_box_0 = content.querySelector('#jspsych-center-box0')
        center_box_1 = content.querySelector('#jspsych-center-box1')
        center_box_2 = content.querySelector('#jspsych-center-box2')
        center_box_3 = content.querySelector('#jspsych-center-box3')
        center_box_4 = content.querySelector('#jspsych-center-box4')
        center_box_5 = content.querySelector('#jspsych-center-box5')
        center_box_6 = content.querySelector('#jspsych-center-box6')
        center_box_7 = content.querySelector('#jspsych-center-box7')

        box_0.innerHTML = default_l[0]
        box_1.innerHTML = default_l[1]
        box_2.innerHTML = default_l[2]
        box_3.innerHTML = default_l[3]
        box_4.innerHTML = default_l[4]

        box_0_r.innerHTML = default_r[0]
        box_1_r.innerHTML = default_r[1]
        box_2_r.innerHTML = default_r[2]
        box_3_r.innerHTML = default_r[3]
        box_4_r.innerHTML = default_r[4]


        center_box_9.innerHTML += '<div class = "problem-center" style = opacity:0;><img src = "img/mult.png" width="110" height="70"></div'
        center_box_0.innerHTML += default_center
        center_box_1.innerHTML += default_center
        center_box_2.innerHTML += default_center
        center_box_3.innerHTML += default_center
        center_box_4.innerHTML += default_center


        if (trial.horizon==long_horizon){
          box_5.innerHTML += default_l[5]
          box_6.innerHTML += default_l[6]
          box_7.innerHTML += default_l[7]
          box_5_r.innerHTML += default_r[5]
          box_6_r.innerHTML += default_r[6]
          box_7_r.innerHTML += default_r[7]
          center_box_5.innerHTML += default_center
          center_box_6.innerHTML += default_center
          center_box_7.innerHTML += default_center
        }
      }

      if (stage == 4) { // points guess panal
        cent_stim.innerHTML = stims
        cent_stim.innerHTML += ('<br><p style="border:4px dotted #d3d3d3;font-family:Serif;line-height: 80px;">'+points_guess+'</p><br>')
        cent_stim.innerHTML += ('<p style="font-size:25px">Clear answer:<br>"Backspace" key<br>Continue:<br>space</p>')
      }

      if (stage == 5) { // points guess feedback
        if (keypress==70){
          b_color = selected_color_l
          rew = trial.rews_a[current_trial]
          trial_hist_l.push('+'+rew)
          trial_hist_r.push(nr)
          current_right.innerHTML  = '<div style=background-color:'+selected_color_r+';color:black;">'+nr+'</div>'
          if (trial.practice_block == 1 || current_trial>3){current_right.innerHTML += r_lever_up}
          bandit_side = 'left'
        } else{
          b_color = selected_color_r
          rew = trial.rews_b[current_trial]
          trial_hist_r.push('+'+rew)
          trial_hist_l.push(nr)
          current_left.innerHTML  = '<div style=background-color:'+selected_color_l+';color:black;">'+nr+'</div>'
          if (trial.practice_block == 1 ||current_trial>3){current_left.innerHTML += l_lever_up}
          bandit_side = 'right'
        }
        current_side.innerHTML  = '<div style=background-color:'+b_color+';color:black;">+'+rew+'</div>' //points side and side color of border
        current_side.innerHTML += '<div class = "'+bandit_side+'down"><div class = "'+bandit_side+'circle"></div></div>'
      }
      if (stage == 6){
        if (correct == 1){
          cent_stim.innerHTML = stims
          cent_stim.innerHTML += ('<br><p style="border:4px dotted green;font-family:Serif;line-height: 80px;">'+points_guess+'</p><br>')
          cent_stim.innerHTML += '<br><p style="font-size:25px;color:green;">Correct</p>'
        }else{
          cent_stim.innerHTML = stims
          cent_stim.innerHTML += ('<br><p style="border:4px dotted red;font-family:Serif;line-height: 80px;">'+points_guess+'</p><br>')
          cent_stim.innerHTML += '<br><p style="font-size:25px;color:red;">Incorrect</p>'
        }
      }

    }
    var start_response_listener = function(){

      if (pause == 0) {
        if (trial.practice_block == 2 && current_trial<4){
          if (forced_choice[current_trial] =='left'){
            choices = [70]
          }else{
            choices = [74]
          }
        }else if (trial.practice_block == 3){
          choices = all_choices[1]
        }else{
          choices = [70,74]
        }
      } else {
        choices = all_choices[0]
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

      display_stimuli(1)

      current_left = content.querySelector('#jspsych-box'+current_trial+'-l')
      current_right = content.querySelector('#jspsych-box'+current_trial+'-r')
      current_center = content.querySelector('#jspsych-center-box'+current_trial)
      if (trial.practice_block == 2 && current_trial<4){

        current_side = content.querySelector('#jspsych-box'+current_trial+'-'+forced_choice[current_trial][0])
        current_center.innerHTML = '<img src = "img/'+forced_choice[current_trial]+'_arrow.png" width="110" height="70">'
        current_side.innerHTML = '<div style=background-color:MediumSeaGreen;color:MediumSeaGreen;">XX</div>'
        current_side.innerHTML += '<div class = "'+forced_choice[current_trial]+'up"><div class = "'+forced_choice[current_trial]+'circle"></div></div>'

      }else if(trial.practice_block == 3){

        display_element.innerHTML = '<div id="jspsych-content"></div>'
        content = display_element.querySelector('#jspsych-content')

        content.innerHTML += '<div id= "jspsych-center-stim"></div>'
        cent_stim = content.querySelector('#jspsych-center-stim')

        stim_1 = '<p style="font-family:Serif;">'+trial.stimulus_one+'</p>'
        stim_2 = '<p style="font-family:Serif;">'+trial.stimulus_two+'</p>'

        add_sltn = trial.stimulus_one+trial.stimulus_two
        mult_sltn = trial.stimulus_one*trial.stimulus_two

        if (trial.condition == '+'){
          correct_answer = trial.stimulus_one+trial.stimulus_two
          problem_type = 'add'
        }else{
          correct_answer = trial.stimulus_one*trial.stimulus_two
          problem_type = 'mult'
        }
        stims = '<br><br><div style="font-family:Serif;"class="pleft_val"><span>'+trial.stimulus_one+'</span></div><div class="pright_val">'+trial.condition+'</div><div style="font-family:Serif;"class="pright_val"><span>'+trial.stimulus_two+'</span></div><br>'
        cent_stim.innerHTML = stims
        cent_stim.innerHTML += ('<br><p style="border:4px dotted #d3d3d3;font-family:Serif;line-height: 80px;">'+points_guess+'</p><br>')
        cent_stim.innerHTML += ('<p style="font-size:25px">Clear answer:<br>"Backspace" key<br>Continue:<br>space</p>')
      }else{

        current_left.innerHTML += '<div class = "leftup"><div class = "leftcircle"></div></div>'
        current_right.innerHTML += '<div class = "rightup"><div class = "rightcircle"></div></div>'
      }
      start_response_listener()
    }

    if (current_trial!=trial.horizon){
      next_trial()
    }
  }

  return plugin
})()
