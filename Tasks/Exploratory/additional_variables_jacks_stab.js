//Meta-explore additional variables

// Jack's stab at it:

function createInstrPage(text){
    return ["<div align=center><div style = 'width:800px'><br>" + text + "<br></div>"]
}

var instructions1a = [
    "Welcome to the experiment!<br><br> The instructions are somewhat lengthy, but they're also necessary to do the task well, and we've built the amount of time they take into the full hour that you'll receive credit for.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br>",
    "Today's experiment will be one of point maximization. Throughout, you'll see two columns of what are essentially slot machines. Slot machines are variable, but in this case we've rigged them so each have an average value of how much reward they yield, and we've also added variability so they might not yield that average amount of reward every time you pull their lever. Each time you pull a slot machine, you'll amass between 1-100 points.",
    "To maximize points, which is the ultimate goal, you'll want to choose which of the two slot machines will give you the most reward. You'll be able to sample each of the two slot machines, but you'll only have a limited number of slot machine pulls, so you'll have to make due with limited information without potentially ever fully knowing the underlying point average of each slot machine.",
    "Let's practice! On the next screen, you'll have seven pulls. Use the F key to sample the left slot machine for any one of those 8 pulls, or the J key to sample the right slot machine.",
    "Great! The double XX's you saw were to further reinforce that for each pull you'll get information about one slot machine, but not the other one. You don't know what you don't attempt to pull, so to speak!",
    "Just to further illustrate, this is what the actual underlying values had all been had you been able to see beneath the double XX's. And just for reference, the true averages for each of the two slot machines were __ for the left one and __ for the right one.<br><br>You won't ever know what the true averages are when you do the main part of the task, but you can clearly see that there's both variability and an overall range for each slot machine, and that they're different!",
    "What you saw is going to be the general setup for the rest of the task, but there's just going to be a few wrinkles, which we'll tell you about now.<br><br>First of all, the first four lever pulls will not be a free choice, but rather a forced choice. So we'll basically control which information you're exposed to in the first four pulls, and therefore that reward won't factor into how much reward you get in the end (we're going to give the top 7 point earners each a $15 gift card on top of participation credit, but again these four first forced trials won't count towards that total).",
    "After those four forced choices, you will have either 1 or 4 lever pulls, for which you are free to choose however you wish.",
    "Let's practice! For the forced choices, the box of one of the two machines will turn green, cueing you to pull that lever. It'll disappear after the four forced choices.",
    "Well done! Another one of the wrinkles is how you select a lever to pull. So far, you've just pressed either F or J. That'll stay true for the forced choices. But for the remaining pulls, you'll have to complete either an addition or multiplication problem as a proxy for pressing F or J.",
    "To get a gist for the math problems, you'll see 10 problems one-by-one. Please, please don't use a calculator! We've gone to lengths to make the experiment's length last less than an hour, and that includes budgeting time to think through the math problems.",
    "When you see the math problems in the context of the slot machines, you won't get a second chance to correctly answer an incorrect response, and that means you simply will see double XX's for both slot machines, like so:",
    "Just as before--when you only needed to press the F and J keys--now solving a problem with multiplication will be the equivalent of using the F key, and addition the equivalent of the J key. So you will always be presented with two numbers, and it'll be up to you whether you want to add those numbers or multiply them. You can type your answer in the box below the numbers, and then we'll automatically check whether your response was for an addition or multiplication problem.",
    "That's all there is to it! Each one of these iterations of 5 or 8 machine pulls we can call a 'game.' You'll complete ___ games during the study, which should last less than ___ minutes.<br><br>Critically, the average value of each slot machine is both unrelated to the other one during the same game, as well as to all future slot machines on either side. So while solving with multiplication will always act on the left slot machine, the left slot machine’s average value in one game is completely unrelated to the average value of the machine on that same (or the other) side during the next game (same goes for the right side machines).",
    "And again, to encourage you to amass many points, those with the top-7 most points earned will win a $15 Amazon gift card, in addition to receiving study participation credit. Press space to begin!"
].map(x => createInstrPage(x))

// Bettina's instructions:
// //Instructions: W said - remove word "stimuli", and avoid using "trial" or "trial history"
// var instructions_1a_text = ["<div align=center><div style = 'width:800px'><br>Welcome!<br><br> Thank you for participating in this experiment.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br><br></div>"]
// var instructions_1b_text = ["<div align=center><div style = 'width:800px'><br>In this experiment you will see stimuli like these:<br><img style='width:100%;'src='img/sample_slot.png'/><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br><br></div>"]
// var instructions_1c_text = ["<div align=center><div style = 'width:850px'><br>Each one of these stimuli represents a slot machine.  At any one time, you  may choose to play one of the two slot machines for the chance to win between 1 and 100 points.<br><br><br></div>"]
// var instructions_1d_text = ["<div align=center><div style = 'width:800px'><br>For example, in this case, the left slot machine is paying out 77 points.<br><img style='width:100%;'src='img/sample_rew_1.png'/><br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1e_text = ["<div align=center><div style = 'width:800px'><br>Your goal is to choose between the slot machines to maximize your reward.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1f_text = ["<div align=center><div style = 'width:800px'><br>The first four trials of each game be instructed trials where we will tell you which slot machine to choose. Use the <b>F</b> and <b>J</b> keys to choose the slot machine you want to play.<br><br>Choose the <b>F</b> key for the left slot machine. <br>Choose <b>J</b> key for the right slot machine.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1g_text = ["<div align=center><div style = 'width:800px'><br>These instructed trials will be indicated by a black arrow pointing towards one of the options and you must press the corresponding key to choose this option in order to move on to see the reward and move on the next trial.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1h_text = ["<div align=center><div style = 'width:800px'><br>In each game, the average payoff from each slot machine is fixed, but on any given play there is variability in the exact amount.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1i_text = ["<div align=center><div style = 'width:800px'><br>For example, the mean payoff from the left slot machine might be 50 points but on any trial you might receive more or less than 50 points randomly.<br><br>Say 52 points on the first play, 56 on the second and 45 on the third.<br><img style='width:100%;'src='img/sample_rew_2.gif'/><br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1j_text = ["<div align=center><div style = 'width:800px'><br>This variability makes it difficult to figure out which is the best slot machine, but that is exactly what you need to do to maximize your payoff from the game.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1k_text = ["<div align=center><div style = 'width:800px'><br>To help you make your decision, we'll show you the rewards you received from each option in the slot machines.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1l_text = ["<div align=center><div style = 'width:800px'><br>If the game is 8 choices long, the slot machines will look like this:<br><img style='width:100%;'src='img/sample_slot_long.png'/><br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_1m_text = ["<div align=center><div style = 'width:800px'><br><br><br>Press <b>spacebar</b> when you are ready to begin. Good luck!<br></div>"]
//have ps practice forced choice trials and then practice free-choice math problems
// var instructions_2a_text = ["<div align=center><div style = 'width:800px'><br>TEXT<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_2b_text = ["<div align=center><div style = 'width:800px'><br>TEXT<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]
// var instructions_2c_text = ["<div align=center><div style = 'width:800px'><br>TEXT<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br></div>"]

//Bobs instructions:
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'In this experiment you will see stimuli like these:';
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'Each one of these stimuli represents a slot machine.  At any one time, you  may choose to play one of the two slot machines for the chance to win between 1 and 100 points.';
            // i=i+1; ev{i} = 'L_77';       iStr{i} = 'For example, in this case, the left slot machine is paying out 77 points';
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'Your goal is to choose between the slot machines to maximize your reward.';
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'Use the F and J keys to choose the slot machine you want to play.\n Choose the F key for the left slot machine\n Choose J key for the right slot machine';
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'In each game, the average payoff from each slot machine is fixed, but on any given play there is variability in the exact amount. ';
            // i=i+1; ev{i} = 'L_52';       iStr{i} = 'For example, the mean payoff from the left slot machine might be 50 points but on any trial you might receive more or less than 50 points randomly - say 52 points on the first play ...';
            // i=i+1; ev{i} = 'L_56';       iStr{i} = '... 56 on the second ...';
            // i=i+1; ev{i} = 'L_45';       iStr{i} = '... and 45 on the third.';
            // i=i+1; ev{i} = 'bandits';    iStr{i} = 'This variability makes it difficult to figure out which is the best slot machine, but that is exactly what you need to do to maximize your payoff from the game.';
            // i=i+1; ev{i} = 'rh';         iStr{i} = 'To help you make your decision, we''ll also show you the history of rewards from each option in these bars ...';
            // i=i+1; ev{i} = 'rh_R54';     iStr{i} = 'For example, if you had received 54 from the right option on the first trial of a game this is what you would see. ';
            // i=i+1; ev{i} = 'rh_R54_58';  iStr{i} = 'If you play the same option again and see a reward of 58 you see this ...';
            // i=i+1; ev{i} = 'rh_many';    iStr{i} = 'After several plays you might see something like this ... your job is to use this information to get the most number of points over the whole game.';
            // i=i+1; ev{i} = 'blank';      iStr{i} = 'Games can be either 5 or 10 trials long.';
            // i=i+1; ev{i} = 'rh5';        iStr{i} = 'If the game is 5 trials long, the history bars will look like this ...';
            // i=i+1; ev{i} = 'rh10';       iStr{i} = 'If the game is 10 trials long, the history bars will look like this ...';
            // i=i+1; ev{i} = 'blank';      iStr{i} = 'Finally, the first four trials of each game will be instructed trials where we will tell you which option to play.  This will give you some experience with each option before you make your first choice.';
            // i=i+1; ev{i} = 'forced';     iStr{i} = 'These instructed trials will be indicated by a white square surrounding one of the options and you must press the button to choose this option in order to move on to see the reward and move on the next trial. ';
            // i=i+1; ev{i} = 'blank';      iStr{i} = 'Press space when you are ready to begin.  Good luck!';


/* images that need to be preloaded */
images = [
  "img/sample_slot.png",
  "img/sample_slot_long.png",
  "img/sample_rew_1.png",
  "img/sample_rew_2.gif",
  "img/mult_selected.png",
  "img/add_selected.png",
  "img/mult.png",
  "img/add.png",
  "img/right_arrow.png",
  "img/left_arrow.png",
]


var ispc_experience_questions = [
  {prompt: "During the task, how often did you use the hand you do not use to control your mouse to make your responses?", options: ["Always", "Most of the time", "Some of the time","Not at all"],required: true},
  {prompt: "How hard did you find responding with the appropriate key?", options: ["Very difficult", "Difficult", "Moderate","Easy"], required: true}
]
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


function createMemberInNormalDistribution(mean,std_dev){
	return mean + (gaussRandom()*std_dev);
}

function gaussRandom() {
	var u = 2*Math.random()-1;
	var v = 2*Math.random()-1;
	var r = u*u + v*v;
	/*if outside interval [0,1] start over*/
	if(r == 0 || r > 1) return gaussRandom();

	var c = Math.sqrt(-2*Math.log(r)/r);
	return u*c;
}

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
