
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

<! --- R-Forge Logo --- >
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

<h2>Vegan resources</h2>

<p>Here some pointers to vegan resources:
<ul>
<li> The old  
<a href="http://cc.oulu.fi/~jarioksa/softhelp/vegan.html">vegan home page</a> </li>  

<li>The project <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>summary page</strong></a> at R-Forge. </li>

<li>Vegan <a href="http://cc.oulu.fi/~jarioksa/softhelp/FAQ-vegan.html">FAQ</a>.</li>

<li>Vegan tutorial as a <a href="http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf">pdf file</a>.</li>

<li><a href="http://wiki.r-project.org/rwiki/doku.php?id=packages:cran:vegan">R wiki entry</a></li>

</ul>

</body>
</html>
