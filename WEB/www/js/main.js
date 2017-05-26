$(document).ready(function(){


  //------------------------------------//
  //Navbar//
  //------------------------------------//
    	var menu = $('.navbar');
    	$(window).bind('scroll', function(e){
    		if($(window).scrollTop() > 140){
    			if(!menu.hasClass('open')){
    				menu.addClass('open');
    			}
    		}else{
    			if(menu.hasClass('open')){
    				menu.removeClass('open');
    			}
    		}
    	});


  //------------------------------------//
  //Scroll To//
  //------------------------------------//
  $(".scroll").click(function(event){
  	event.preventDefault();
  	$('html,body').animate({scrollTop:$(this.hash).offset().top}, 800);

  });

  //------------------------------------//
  //Wow Animation//
  //------------------------------------//
  wow = new WOW(
        {
          boxClass:     'wow',      // animated element css class (default is wow)
          animateClass: 'animated', // animation css class (default is animated)
          offset:       0,          // distance to the element when triggering the animation (default is 0)
          mobile:       false        // trigger animations on mobile devices (true is default)
        }
      );
      wow.init();


  // Rath code
  var args = [];
  var submitBtn = $('#submitBtn');
  var sex = $('input:radio[name=sex]:checked');
  var age = $('#age');
  var cp = $('#cp');
  var trestbps = $('#trestbps');
  var chol = $('#chol');
  var fbs = $('#fbs');
  var restecg = $('#restecg');
  var thalach = $('#thalach');
  var exang = $('#exang');
  var oldpeak = $('#oldpeak');
  var ca = $('#ca');
  var slope = $('#slope');
  var thal = $('#thal');
  var mainForm = $('#mainForm');
  var goodResult = $('#goodResult');
  var badResult = $('#badResult');
  var testAgain = $('.testAgain');

  $('form').on('submit', function(e){
    e.preventDefault();
    $('body').animate({
        scrollTop: parseInt($('#test').offset().top)
    }, 300);

    args.push('' + age.val());
    args.push('' + sex.val());
    args.push('' + cp.val());
    args.push('' + trestbps.val());
    args.push('' + chol.val());
    args.push('' + fbs.val());
    args.push('' + restecg.val());
    args.push('' + thalach.val());
    args.push('' + exang.val());
    args.push('' + oldpeak.val());

    var obj = {
      parameters: args
    }

    mainForm.hide();
    goodResult.show();
    badResult.show();

    // $.ajax({
    //   type: 'POST',
    //   url: 'https://trial.dominodatalab.com/v1/rathpanyowat/forest/endpoint',
    //   data: JSON.stringify(obj),
    //   headers: {'X-Domino-Api-Key': 'iteNMTDKg2BsL2iE9kuJ7sRctAJE2Q4IEonqtqAiBLWq35ejmpdlTPc2uJL4omin'},
    //   success: function( response ) {
    //     console.log(response);
    //   }
    // });

  });

  testAgain.on('click', function(e){
    e.preventDefault();
    mainForm.show();
    goodResult.hide();
    badResult.hide();
    $('body').animate({
        scrollTop: parseInt($('#test').offset().top)
    }, 300);
  });


});
