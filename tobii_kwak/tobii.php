<?php

// visagetests, cible2.RESP, cible2.CRESP, cible2.ACC, cible2.RT
$source = basename($_FILES['file']['name']);
$uploaded_file = "./files/" . $source;

header("Content-Type: application/vnd.ms-excel");
header("Content-Disposition: attachment; filename=$source.csv");
header("Pragma: no-cache");
header("Expires: 0");

$separator = "/Level: 4/";

echo "Fichier\tcible2.RESP\tcible2.CRESP\tcible2.ACC\tcible2.RT\n";

if (move_uploaded_file($_FILES['file']['tmp_name'], $uploaded_file)) {
  $file_array = file($uploaded_file);
  $training = 1;
  foreach ($file_array as $line_number => $line)
    {
      $line = trim(str_replace("\r", "", $line));
      //$line = mb_convert_encoding($line, "ISO-8859-1", "UTF-16"));
      if ($training == 1) {
	if (preg_match($separator, $line)) {
	  $training=0;
	}
      } else {
	// end training
	if (preg_match("/visagestest: visagestest.(.*)/", $line, $matches)) {
	  $filename = $matches[1];
	}
	if (preg_match("/cible2.RESP: (.*).*/", $line, $matches)) {
	  $resp = $matches[1];
	}
	if (preg_match("/cible2.CRESP: (.*)/", $line, $matches)) {
	  $cresp = $matches[1];
	}
	if (preg_match("/cible2.ACC: (.*)/", $line, $matches)) {
	  $acc = $matches[1];
	}
	if (preg_match("/cible2.ACC: (.*)/", $line, $matches)) {
	  $acc = $matches[1];
	}
	if (preg_match("/cible2.RT: (.*)/", $line, $matches)) {
	  $rt = $matches[1];
	}
	if (preg_match($separator, $line)) {
	  echo "$filename\t$resp\t$cresp\t$acc\t$rt\n";
	}
      }
    }
} else {
  echo "Argh ! Erreur.";
}
?>