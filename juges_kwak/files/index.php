<html>
<head>
<title>Gestion des fichiers son</title>
</head>
<body style="color: rgb(0, 0, 0); background-color: rgb(66, 60, 60);" alink="#ff0000" link="#ffcc00" vlink="#ffcc33">
<span style="color: rgb(255, 255, 204);">
<h2>Gestion des fichiers son</h2>

<?php
$uploaddir = "../sounds/";
date_default_timezone_set("Europe/Paris"); 
if(isset($_POST['fileUpload']))
{
  foreach($_FILES["uploadFiles"]["error"] as $key => $error) 
    {
      if($error == UPLOAD_ERR_OK) 
        {
	  $tmpFilename = $_FILES["uploadFiles"]["tmp_name"][$key];
	  $filename = $_FILES["uploadFiles"]["name"][$key];
	  $permFilename = $uploaddir.$filename;
            $filenames .= $filename."\r\n";
	  move_uploaded_file($tmpFilename,$permFilename) or die("Failed to upload file: $permFilename");
        }
    }
}
?>



<form action="" method="post" name="uploadForm" enctype="multipart/form-data">
<table id="formTable" border=0>
<tr><td style="color: rgb(255, 255, 204);">Fichier 1</td><td><input type="file" name="uploadFiles[]" /></td></tr>
<tr><td style="color: rgb(255, 255, 204);">Fichier 2</td><td><input type="file" name="uploadFiles[]" /></td></tr>
<tr><td style="color: rgb(255, 255, 204);">Fichier 3</td><td><input type="file" name="uploadFiles[]" /></td></tr>
<tr><td style="color: rgb(255, 255, 204);">Fichier 4</td><td><input type="file" name="uploadFiles[]" /></td></tr>
<tr><td style="color: rgb(255, 255, 204);">Fichier 5</td><td><input type="file" name="uploadFiles[]" /></td></tr>
<tr><td>&nbsp</td><td><input type="submit" value="Envoyer" /></td></tr>
</table>
<input type="hidden" name="fileUpload">
</form>


<br><br>
<?php
if(isset($_GET['dfile']))
{
    $filename=stripslashes($_GET['dfile']);
    $delFilename = $uploaddir . $filename;
    if(file_exists($delFilename))
    {
        if(unlink($delFilename))
        {
            print "<br>$filename a été supprimé.";
        }
        else
        {
            print "<br>$filename n'a pas été supprimé.";
        }
    }
    else
    {
        print "<br>$filename n'a pas été trouvé.";
    }
}
?>

<?php
$sounds=array();
$handle = opendir($uploaddir);
while ($file = readdir($handle)) {
  if (preg_match("/.wav$/", $file))  $sounds[]=$uploaddir.$file;
}
sort($sounds);
$count = count($sounds);

print "<table border=1>\n";
print "<th style=\"color: rgb(255, 255, 204);\">Nom</th><th style=\"color: rgb(255, 255, 204);\">Taille</th><th style=\"color: rgb(255, 255, 204);\">Date</th>";
for ($i=0;$i<$count;$i++)
{
  $file = $sounds[$i];
  $filename = $uploaddir.$file;
  print "<tr>\n";
  print "<td><a href=\"$filename\">$file</a></td><td>".filesize($filename)."</td><td>".date("F d Y H:i:s.", filectime($filename))."</td><td><a href=index.php?dfile=".urlencode($file).">Supprimer</a></td>\n";
  print "</tr>\n";
}
closedir($handle);
print "</table>\n";
?>
</body>
</html>