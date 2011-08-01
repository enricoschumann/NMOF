
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
    <meta name="keywords" content="Heuristics, Differential Evolution, Particle Swarm Optimisation, Threshold Accepting, Portfolio optimisation, Option pricing">
    <title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>
<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" width="125" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<!--
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>


-->


<h2>Numerical Methods and Optimization in Finance</h2>

<img src = "http://enricoschumann.wikidot.com/local--files/table-of-contents/book.jpg" width="15%" align="left" vspace="5" hspace="20"/>
<p>
The book explains tools for computational finance. It covers fundamental numerical analysis and computational techniques, for example for option pricing, but two topics are given special attention: simulation and optimization. Many chapters are organized as case studies, dealing with problems like portfolio insurance or risk estimation; in particular, several chapters explain optimization heuristics and how to use them for portfolio selection or the calibration of option pricing models. Such practical examples allow readers to learn the required steps for solving specific problems, and to apply these steps to other problems, too. At the same time, the chosen applications are relevant enough to make the book a useful reference on how to handle given problems. Matlab and R sample code is provided in the text and can be downloaded from the book's website; an R-package 'NMOF' is also available (see below).
</p>



<p> For more information about the book, please visit the publisher's website <a href="http://www.elsevierdirect.com/ISBN/9780123756626/Numerical-Methods-and-Optimization-in-Finance"><strong>here</strong></a>. </p>
<p></p>
<p> To get the 'NMOF' package, go <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
