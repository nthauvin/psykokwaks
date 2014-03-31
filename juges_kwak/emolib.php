<?php

include("wavlib.php"); // write stats

$id=$_GET['id'];
$ts=$_GET['ts'];
$code=$_GET['code'];
$step=$_GET['step'];
$age=$_GET['age'];
$sexe=$_GET['sexe'];
if(isset($_GET['emotion'])) { $emotion=$_GET['emotion'];};
if(isset($_GET['force'])) { $force=$_GET['force'];};

$sounds=array();
$dirname="sounds/";
$handle=opendir($dirname);
while ($file = readdir($handle)) {
  if (preg_match("/.wav$/", $file))  $sounds[]=$dirname.$file;
}
closedir($handle);
$count=count($sounds);
sort($sounds);

if (isset($emotion)) {
  $sent_sound_file=$sounds[$step-1];
  $sound_duration=wavDur($sent_sound_file);
  $File = "$ts-$code.$id.xls";
  $script_directory = substr($_SERVER['SCRIPT_FILENAME'], 0, strrpos($_SERVER['SCRIPT_FILENAME'], '/'));
  $emo_parts = explode("_", $sent_sound_file);
  $emo_code = $emo_parts[1];
  $emo_letter = ucfirst(substr($emo_code, 0, 1));
  if (($emo_letter == "C") && ($emotion == "negative")) $valid = 1;
  else if (($emo_letter == "N") && ($emotion == "neutre")) $valid = 1;
  else if (($emo_letter == "J") && ($emotion == "positive")) $valid = 1;
  else $valid = 0;

  $fh = fopen("$script_directory/out/$File", 'a') or die("can't open file");
  fwrite($fh, "$code\t$id\t$age\t$sexe\t$step\t$sent_sound_file\t$emo_code\t$sound_duration\t$emotion\t$force\t$valid\n");
  fclose($fh);
}



?>