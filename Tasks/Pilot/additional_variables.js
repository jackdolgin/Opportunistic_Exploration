//Meta-explore additional variables


//Combine trial data and slider data
function change_slider_data(trial_data,slider_data) {
  var trial_data_json = JSON.parse(trial_data)
  var slider_data_json = JSON.parse(slider_data)

  var sub_array = []
  var calc_array = []
  var corr_array = []
  var tt_array = []
  var stim1_array = []
  var stim2_array = []
  var acc_array = []
  var res_array = []
  var trial_array = []
  var slidert_array = []
  var math_rt_array = []

  for (var i =0; i< trial_data_json.length; i++) {
        	sub_array.push(trial_data_json[i]['subid'])
          calc_array.push(trial_data_json[i]['calculation'])
          corr_array.push(trial_data_json[i]['correct_calc'])
          tt_array.push(trial_data_json[i]['type'])
          stim1_array.push(trial_data_json[i]['stim_1'])
          stim2_array.push(trial_data_json[i]['stim_2'])
          acc_array.push(trial_data_json[i]['accuracy'])
          res_array.push(slider_data_json[i]['response'])
          slidert_array.push(slider_data_json[i]['rt'])
          trial_array.push(trial_data_json[i]['trial_nr'])
          math_rt_array.push(trial_data_json[i]['math_rt'])
	}
  for (var i =0; i< slider_data_json.length; i++){
    trial_data_json[i]['subid'] = sub_array[i]
    trial_data_json[i]['calculation'] = calc_array[i]
    trial_data_json[i]['correct_calc'] = corr_array[i]
    trial_data_json[i]['type'] = tt_array[i]
    trial_data_json[i]['stim_1'] = stim1_array[i]
    trial_data_json[i]['stim_2'] = stim2_array[i]
    trial_data_json[i]['accuracy'] = acc_array[i]
    trial_data_json[i]['slide_response'] = res_array[i]
    trial_data_json[i]['trial_nr'] = trial_array[i]
    trial_data_json[i]['slider_rt'] = slidert_array[i]
    trial_data_json[i]['math_rt'] = math_rt_array[i]

  }
  return(trial_data_json)
}

//Instructions:
var instructions_1a_text = ["<div align=center><div style = 'width:800px'>Welcome to our task! <br><br>This experiment is going to be extremely straightforward. But that's all it is, no tricks!<br>On each trial you're going to see two numbers between 1 and 24. You will be asked to multiply or add these numbers and we'd like you to type the correct answer as quickly as you can. <br>You can do so in the box that we provide underneath the equation, and you can submit each answer using either the submit button or enter key.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br><br></div>"]
var instructions_1b_text = ["<div align=center><div style = 'width:800px'><br>Obviously, we'd like you to do this without a calculator, so please refrain from using one. <br>The aim of this study is to examine how individuals do math problems in their head. You'll get the same course credit regardless of your respsonse but your data will not be usable if you do not do these problems in your head, so again please don't try to use a calculator.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br><br></div>"]
var instructions_1c_text = ["<div align=center><div style = 'width:850px'>After each problem you'll be asked to rate how difficult you thought the question was for you.<br>There's no wrong answer!<br><br> We just want to know your personal experience, you can make a selection with the provided slider. Please make sure to use the full scale and go with your instinct.<br><br><br></div>"]
var instructions_1d_text = ["<div align=center><div style = 'width:800px'>You'll be provided with time to take a break half way through the experiment. The whole experiment is expected to last 30 minutes.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
var instructions_1d_text = ["<div align=center><div style = 'width:800px'>The task will begin as soon as you continue on to the next screen.<br>Again, please do these problems in your head and do not use a calculator or write the problems down. Make sure to use the full scale and go with your instinct.<br> For each math problem, we will let you know whether you solved it correctly after you give us a response on the slider.<br><br>Press <b>next</b> or <b>spacebar</b> to begin the task.<br><br><br></div>"]

var ispc_experience_questions = [
  {prompt: "During the task, how often did you use the hand you do not use to control your mouse to make your responses?", options: ["Always", "Most of the time", "Some of the time","Not at all"],required: true},
  {prompt: "How hard did you find responding with the appropriate key?", options: ["Very difficult", "Difficult", "Moderate","Easy"], required: true}
]

/* images that need to be preloaded */
images = [
  "img/mult_selected.png",
  "img/add_selected.png",
  "img/mult.png",
  "img/add.png",
]

/* function that saves data in a mysql database */
function save_data(data, table) {
	var xhr = new XMLHttpRequest()
	xhr.open('POST', 'write_data.php') // change 'write_data.php' to point to php script.
	xhr.setRequestHeader('Content-Type', 'application/json')
	xhr.onload = function() {
		console.log(xhr.responseText)
		if(xhr.status == 200){
			var response = JSON.parse(xhr.responseText)
			console.log(response.success)
		}
	}
	xhr.send('data=' + data + '&table=' + table)
}

/* function that allows removal of a specific value */
Array.prototype.remove = function() {
	var what, a = arguments, L = a.length, ax
	while (L && this.length) {
		what = a[--L]
		while ((ax = this.indexOf(what)) !== -1) {
			this.splice(ax, 1)
		}
	}
	return this
}

function getQueryVariable(variable)
{
	var query = window.location.search.substring(1)
	var vars = query.split("&")
	for (var i=0;i<vars.length;i++) {
		var pair = vars[i].split("=")
		if(pair[0] == variable){return pair[1];}
	}
	return(false)
}
