function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start : function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
  slides.pretrial = slide({
   name : "pretrial",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
   }
  });
  
  slides.one_slider_practice = slide({
    name : "one_slider_practice",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */

    present : [
    {type: "practice", item: "all_boat"},
    {type: "practice", item: "all_camera"}
    ],

    //this gets run only at the beginning of the block
    present_handle : function(stim) {
    $(".err").hide();
    $(".hidden").hide();
    // $(".option").unbind("click");
    // $(".option").empty();
    $(".option").hide();
    this.init_sliders();
    exp.sliderPost = null;    

    this.stim = stim; //I like to store this information in the slide so I can record it later.

      exp.item = stim["item"]


      exp.answer = stim.sentence
      $("#answer_practice").html(exp.answer);

      exp.video = '/<source src = "videos/' + exp.item + '.mp4" type="video/mp4"></source>'

      $("#practiceVideo").html(exp.video);
      $("#practiceVideo").load();
      document.getElementById("practiceVideo").onended = function() {     
          exp.startTrialDate = new Date();
          exp.startTrialTime = exp.startTrialDate.getTime()
        right()};
      function right() {
      $(".hidden").show()
      $(".option").show()
      }
      // this.init_sliders();
      // exp.sliderPost = null;   //erase current slider value

      this.n_sliders = 1;

    },


    button : function() {
        console.log(exp.sliderPost);
        if (exp.sliderPost != null) {
          this.endTrialDate = new Date();
          this.endTrialTime = this.endTrialDate.getTime()
          this.log_responses();
          _stream.apply(this); //use exp.go() if and only if there is no "present" data.
        } else {
          $(".err").show();
        }
      },

      init_sliders : function() {
        utils.make_slider("#slider0_practice", function(event, ui) {
          exp.sliderPost = ui.value;
        });
      },

      log_responses : function() {
        exp.data_trials.push({
                "response" : exp.sliderPost,
                "type" : "practice",
  //              "scope" : exp.scope,
  //              "objectType" : exp.objectType,
  //              "topicType" : exp.topicType,
                "item" : exp.item,
                "itemType" : this.stim.type,
                "slide_number" : exp.phase,
                "startTime" : exp.startTrialTime,
                "endTime" : this.endTrialTime,
                "responseTime" : this.endTrialTime - exp.startTrialTime
              });
      }

  });
  
  /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */

  some =
   _.shuffle(
     [
       {type: "test", item: "some_camera", quantifier: "some"},
       {type: "test", item: "some_circle", quantifier: "some"},
       {type: "test", item: "some_flower", quantifier: "some"},
       {type: "test", item: "some_house", quantifier: "some"},
     ]
    );

    all =
     _.shuffle(
      [
        {type: "test", item: "all_circle", quantifier: "all"},
        {type: "test", item: "all_flag", quantifier: "all"},
        {type: "test", item: "all_flower", quantifier: "all"},
        {type: "test", item: "all_heart", quantifier: "all"},
        {type: "test", item: "all_key", quantifier: "all"},

      ]
     );

    notall =
     _.shuffle(
      [
        {type: "test", item: "notall_ball", quantifier: "notall"},
        {type: "test", item: "notall_cake", quantifier: "notall"},
        {type: "test", item: "notall_cup", quantifier: "notall"},
        {type: "test", item: "notall_doughnut", quantifier: "notall"},
        {type: "test", item: "notall_flower", quantifier: "notall"},
      ]
     )
    ;

    or =
     _.shuffle(
      [
        {type: "test", item: "or_arrowTree", quantifier: "or"},
        {type: "test", item: "or_boat", quantifier: "or"},
        {type: "test", item: "or_cupCake", quantifier: "or"},
        {type: "test", item: "or_flowerpotHouse", quantifier: "or"},
        {type: "test", item: "or_icecreamSun", quantifier: "or"},
      ]
     );

    two = 
     _.shuffle(
      [
        {type: "test", item: "two_arrow", quantifier: "two"},
        {type: "test", item: "two_ball", quantifier: "two"},
        {type: "test", item: "two_balloon", quantifier: "two"},
        {type: "test", item: "two_cloud", quantifier: "two"},
        {type: "test", item: "two_cross", quantifier: "two"},

      ]
     );

    var listOfList = [all, notall, some, or, two];
    shuffledIndices = _.shuffle([0,1,2,3,4]);
    var shuffledItems = []; 
    for (i = 0; i < 5; i++) {
      shuffledItems = shuffledItems.concat(listOfList[shuffledIndices[i]]);
    }


  slides.one_slider = slide({
    name : "one_slider",
    present : shuffledItems,
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
		$(".err").hide();
		$(".hidden").hide();
    // $(".option").unbind("click");
    // $(".option").empty();
    $(".option").hide();
    this.init_sliders();
    exp.sliderPost = null;    

		this.stim = stim; //I like to store this information in the slide so I can record it later.
      exp.item = stim["item"]

      exp.answer = stim[exp.scope]
      $("#answer").html(exp.answer);

      if (stim["type"] == "test") {
        exp.video = '/<source src = "videos/' + exp.item + '.mp4" type="video/mp4"></source>'
      } else {
        exp.video = '/<source src = "videos/' + exp.item + '.mp4" type="video/mp4"></source>'
      }

      $("#expVideo").html(exp.video);
  	  $("#expVideo").load();
  	  document.getElementById("expVideo").onended = function() {     
          exp.startTrialDate = new Date();
          exp.startTrialTime = exp.startTrialDate.getTime()
        right()};
  		function right() {
  			$(".hidden").show()
        $(".option").show()
  		}
     
      this.n_sliders = 1;

    },


    button : function() {
        console.log(exp.sliderPost);
        if (exp.sliderPost != null) {
          this.endTrialDate = new Date();
          this.endTrialTime = this.endTrialDate.getTime()
          this.log_responses();
          _stream.apply(this); //use exp.go() if and only if there is no "present" data.
        } else {
          $(".err").show();
        }
      },

      init_sliders : function() {
        utils.make_slider("#slider0", function(event, ui) {
          exp.sliderPost = ui.value;
        });
      },

      log_responses : function() {
        exp.data_trials.push({
                "response" : exp.sliderPost,
                "type" : "trial",
     //          "scope" : exp.scope,
     //           "objectType" : exp.objectType,
     //           "topicType" : exp.topicType,
                "item" : exp.item,
                "itemType" : this.stim.type,
                "slide_number" : exp.phase,
                "startTime" : exp.startTrialTime,
                "endTime" : this.endTrialTime,
                "responseTime" : this.endTrialTime - exp.startTrialTime
              });
      }
       //use exp.go() if and only if there is no "present" data.
  });
  

 

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        assess : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
		// selfreport : $("#selfreport").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
		  "justification" : exp.justify,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  repeatWorker = false;
  (function(){
      var ut_id = "scopeTVJT-fixed";
      if (UTWorkerLimitReached(ut_id)) {
        $('.slide').empty();
        repeatWorker = true;
        alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
      }
  })();

  exp.trials = [];
  exp.catch_trials = [];
  //exp.condition = _.sample(["Cond 1"]); //can randomize between subject conditions here
  //exp.condition = _.sample(["Cond 1, Cond 2, Cond 3, Cond 4"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width,
    };
  //blocks of the experiment:
  // exp.structure=["i0", "instructions", "one_slider_practice", "pretrial", "one_slider", 'subj_info', 'thanks'];
  exp.structure=[
                "i0",
                "instructions",
                "one_slider_practice",
                "pretrial",
                "one_slider", 
                'subj_info', 
                'thanks'
                ];
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);
	
	//exp.nQs = 2;
  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
