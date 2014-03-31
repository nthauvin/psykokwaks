<?php
$id=$_GET['id'];

include("emolib.php");

$html= <<< EOH
<!DOCTYPE html>
<html>
 <head>
  <meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />
  <title>Fin</title>
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
 </head>
 <body style="font-family:Arial;width:600px;">
  <h1>Terminé !</h1>
  <p>Merci de votre participation, $id.</p>
  <p>Fichier résultat <a href="out/$ts-$code.$id.xls">ici</a>
  </p>
 </body>
</html>
EOH;
echo "$html";
?>