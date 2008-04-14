
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->
<h2>What is in vegan?</h2>

<p>Vegan contains many popular tools used in community ecology. The following list is not exhaustive, but lists only some popular tools:

<ul>
<li><strong>Diversity analysis:</strong> Shannon, Simpson, Fisher indices, Rényi diversities and Hill numbers.</li>
<li><strong>Species abundance models:</strong> Fisher and Preston models, species abundance distributions.</li>
<li><strong>Analysis of species richness:</strong> species accumulation curves, extrapolated richness.</li>
<li><strong>Ordination:</strong> support and meta functions for NMDS, redundancy analysis, constrained correspondence analysis, constrained analysis of proximities (all three with partial analysis), </li>
<li><strong>Support functions for ordination:</strong> dissimilarity indices, extended dissimilarities, Procrustes analysis, ordination diagnostics, permutation tests.</li>
<li><strong>Ordination and environment:</strong> vector fitting, centroid fitting and smooth surface fitting, adding species scores as weighted averages, adding convex hull, SD ellipses, arrows etc. to ordination.</li>
<li><strong>Dissimilarity analyses:</strong> ANOVA using dissimilarities, ANOSIM, MRPP, BIOENV, Mantel and partial Mantel tests.</li>
<li><strong>Data standardization:</strong> Hellinger, Wisconsin, Chi-square, Beals smoothing.</li>
</ul>

<p>
<a href="http://<?php echo $domain; ?>/scm/?group_id=68">
<img src="http://<?php echo $domain; ?>/images/downloadbar.gif" alt="DOWNLOAD" border="0"></a>

<h2>Vegan resources</h2>

<p>Here some pointers to vegan resources:
<ul>
     <li>Developer diaries: <a href="http://<?php echo $domain; ?>/developer/diary.php?diary_user=267">Jari Oksanen</a>, <a href="http://<?php echo $domain; ?>/developer/diary.php?diary_user=269">Gavin Simpson</a></li>
<li> The old  
<a href="http://cc.oulu.fi/~jarioksa/softhelp/vegan.html">vegan home page</a> </li>  

<li>The project <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>summary page</strong></a> at R-Forge. </li>

<li>Vegan <a href="http://cc.oulu.fi/~jarioksa/softhelp/FAQ-vegan.html">FAQ</a>.</li>

<li>Vegan tutorial as a <a href="http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf">pdf file</a>.</li>

<li><a href="http://wiki.r-project.org/rwiki/doku.php?id=packages:cran:vegan">R wiki entry</a></li>

</ul>

<hr>

$Date$

</body>
</html>
