<?php
include("emolib.php"); // write stats

if ($step >= ($count-1)) {
  $next_action="done.php";
} else {
  $next_action="listen.php";
}

$sound=$sounds[$step];
$next_step=$step+1;

$html= <<< EOH
<!DOCTYPE html>
<html>
 <head>
  <meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />
  <title>Phrase $next_step / $count</title>
   <script type="text/javascript" src="jquery-1.11.0.min.js"></script>
   <script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-27538348-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

  </script>
  <style>body {font-family:Arial;width:600px;}</style>
 </head>
 <body>
  <h1>Phrase $next_step / $count</h1>
  <div>
<span style="float:left;padding-right:40px;padding-top:5px;">Ecouter la phrase :</span>
 <object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" id="player">
    <embed src="wavplayer.swf?gui=mini&amp;h=40&amp;w=40&amp;sound=$sound&4"
        bgcolor="#ffffff"
        width="20"
        name="player"
        height="20"
        allowScriptAccess="always"
        type="application/x-shockwave-flash"
        pluginspage="http://www.macromedia.com/go/getflashplayer"
    />
</object>
  </div>

  <script type="text/javascript">

   function is_selected(field_name) {
    return ($('.' + field_name + ':checked').length > 0);
   }

   function validate() {
     if(is_selected('emotion') && is_selected('force')) { return true;}
     else {
       alert('Veuillez remplir tout le questionnaire');
       return false;
     }
  };
</script>

  <form action="$next_action" method="get" name="questions">
    <ul>
      <li>
	<div>
	  <p>Quelle est l'émotion ? (en vous fiant à l'intonation de la voix)?</p>
	  <input type="radio" name="emotion" class="emotion" value="negative"/>Negative
	  <input type="radio" name="emotion" class="emotion" value="neutre"/>Neutre
	  <input type="radio" name="emotion" class="emotion" value="positive"/>Positive
	</div>
	<div style="margin-top:10px;display:none;" class="emotion_details" id="negative_details">
	  Plus précisemment, indiquez son intensité, de subtile (-1) à
	  plus nette (-4) :
	  <p>
	    (Subtile)
	    <input type="radio" name="force" class="force" value="-1"/>-1
            <input type="radio" name="force" class="force" value="-2"/>-2
            <input type="radio" name="force" class="force" value="-3"/>-3
	    <input type="radio" name="force" class="force" value="-4"/>-4
	    (Plus nette)
	  </p>
	</div>
	<div style="margin-top:10px;display:none;" class="emotion_details" id="positive_details">
	  Plus précisemment, indiquez son intensité, de subtile (1) à
	  plus nette (4) :
	  <p>
	    (Subtile)
	    <input type="radio" name="force" class="force" value="1"/>1
            <input type="radio" name="force" class="force" value="2"/>2
            <input type="radio" name="force" class="force" value="3"/>3
	    <input type="radio" name="force" class="force" value="4"/>4
	    (Plus nette)
	  </p>
	</div>
	<input type="radio" name="force" class="force" id="force_neutre" style="display:none" value="0"/>
      </li>
    </ul>

    <input type="hidden" name="code" value="$code"/>
    <input type="hidden" name="id" value="$id"/>
    <input type="hidden" name="ts" value="$ts"/>
    <input type="hidden" name="age" value="$age"/>
    <input type="hidden" name="sexe" value="$sexe"/>
    <input type="hidden" name="step" value="$next_step"/>

    <hr style="margin:20px 0 20px 0;"/>
    <input type="submit" value="Suite" onclick="return validate();"/>
  </form>
<script type="text/javascript">
$(".emotion").click(function() {
    $(".emotion_details").hide();
    $(".force").attr("checked", false);
    switch ($(this).val()) {
    case 'negative':
      $("#negative_details").show(400);break;
    case 'positive':
      $("#positive_details").show(400);break;
    case 'neutre':
      $("#force_neutre").prop("checked", true);break;
    }
});

</script>

 </body>
</html>
EOH;
echo "$html";
?>

