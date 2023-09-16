//Metaexplore additional variables

//Instructions
function createInstrPage(text){
  return ["<div align=center><div class = 'center_text'; style = 'width:800px';>" + text + "<br></div>"]

}

var instructions1 = [
  "Welcome to the experiment!<br><br> The instructions are somewhat lengthy, but they're also necessary to do the task well, and we've built the amount of time they take into the full hour that you'll receive credit for.<br><br>Press <b>next</b> or <b>spacebar</b> to continue.<br><br>",
  "Today's experiment will be one of point maximization. Throughout, you'll see two columns that are each essentially one slot machine. The two slots machines will differ in how much reward they yield on average. Also, whenever you pull a slot machine, there will be variability in how much reward you get. So, you will sometimes get less than its average value, and sometimes more. Each time you pull a slot machine, you'll collect between 1-100 points.",
  // Additionally, the average values of each slot machine change throughout the task.
  "To maximize points, which is the ultimate goal, you'll want to choose which of the two slot machines will give you the most reward. You'll be able to sample each of the two slot machines, but you'll only have a limited number of slot machine pulls, so you'll have to make do with limited information without potentially ever fully knowing the underlying point average of each slot machine.",
  "Let's practice! On the next screen, you'll have eight pulls. Use the F key to sample the left slot machine for any one of those 8 pulls, or the J key to sample the right slot machine.",
].map(x => createInstrPage(x))

var instructions2a = [
  "Great! The double XX's you saw were to further reinforce that for each pull you'll get information about one slot machine, but not the other one. You don't know what you don't attempt to pull, so to speak!",
  ].map(x => createInstrPage(x))

  var instructions2b = [
    "<div align=center><div style = 'width:800px';>Just to further illustrate, this is what the actual underlying values had all been had you been able to see beneath the double XX's. And just for reference, the true averages for each of the two slot machines were 61 for the left one and 44 for the right one.<br><img style='width:60%;'src='img/example_horizon_8.png'/><br><br>You won't ever know what the true averages are when you do the main part of the task, but you can clearly see that there's both variability and an overall range for each slot machine, and that they're different!<br></div>",
  ]

var instructions2c = [
  "What you saw is going to be the general setup for the rest of the task, but there's just going to be a few wrinkles, which we'll tell you about now.<br><br>First of all, the first four lever pulls will not be a free choice, but rather a forced choice. So we'll control which information you're exposed to in the first four pulls, and therefore that reward won't factor into how much reward you get in the end (we're going to give the top 7 point earners each a $15 gift card on top of participation credit, but again these first four forced trials won't count towards that total).",
  "After those four forced choices, you will have either 1 or 4 lever pulls left, for which you are free to choose however you wish.",
  "Let's practice! For the forced choices, the box of one of the two machines will turn green, and you will see a black arrow cueing you to pull that lever. It'll disappear after the four forced choices.",
].map(x => createInstrPage(x))

function instructions3(){
  return[
    "Well done! Another one of the wrinkles is how you select a lever to pull. So far, you've just pressed either F or J. That'll stay true for the forced choices. But for the remaining pulls, you'll have to complete either a "+left_arithmetic+" or "+right_arithmetic+" problem as a proxy for pressing F or J.",
    "To get a gist for the math problems, you'll see 10 problems one-by-one. Please, please don't use a calculator or notepad! We've gone to lengths to make the experiment's duration last less than an hour, and that includes budgeting time to think through the math problems so again, <br><b>it is very important that you do all calculations in your head<b>.",
  ].map(x => createInstrPage(x))}

function instructions4(){
  return[
    "When you see the math problems in the context of the slot machines, you won't get a second chance to correctly answer an incorrect response, and that means you simply will see double XX's for both slot machines, like so:<br><br><img style='width:60%;'src='img/xxsample.png'/>",
    "Just as before -- when you only needed to press the F and J keys -- now solving a problem with "+left_arithmetic+" will be the equivalent of using the F key, and "+right_arithmetic+" the equivalent of the J key. <br><br>So you will always be presented with two numbers, and it'll be up to you whether you want to add those numbers or multiply them. You can type your answer in the box below the numbers, press spacebar to submit and then we'll automatically check whether your response was for an addition or multiplication problem.",
    "That's all there is to it! Each one of these iterations of 5 or 8 machine pulls we can call a 'game'. Press next to practice a game of each length before moving on to the main task."
  ].map(x => createInstrPage(x))}

function instructions5(){
  return[
    "You just practiced a game with 5 pulls, press next to practice a game with 8 pulls."
  ].map(x => createInstrPage(x))}

function instructions6(){
  return[
    "Great! You'll complete 80 games during the study, which should last less than 60 minutes.<br><br>Critically, the average value of each slot machine is both unrelated to the other one during the same game, as well as to all future slot machines on either side. So while solving with "+left_arithmetic+" will always act on the left slot machine, the left slot machine's average value in one game is completely unrelated to the average value of the machine on that same (or the other) side during the next game (same goes for the right side machines).",
    "And again, to encourage you to amass many points, those with the top-7 most points earned will win a $15 Amazon gift card, in addition to receiving study participation credit. Press space to begin!"
  ].map(x => createInstrPage(x))}
        /* images that need to be preloaded */
        images = [
          "img/xxsample.png",
          "img/example_horizon_8.png",
          "img/example_horizon_9.png",
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
