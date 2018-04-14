<?php

if ("${_SERVER['QUERY_STRING']}" == "")
  $self = "${_SERVER['PHP_SELF']}";
else
  $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";

session_start();

function random () {
	return rand(10000, 1000000000);
}

function __eval ($program) {
	$data = trim($program, "\r\n") . "\r\n";
    
	$ch = curl_init();
	// curl_setopt($ch, CURLOPT_URL, 'http://courses.softlab.ntua.gr/pl2/2016b/exercises/befunge93-api/?restrict');
	curl_setopt($ch, CURLOPT_URL, "http://127.0.0.1:5000/befunge93-api/?restrict");
	curl_setopt($ch, CURLOPT_POST, true);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
	curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: text/plain")); 
	curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
    $output = curl_exec($ch);
    $_SESSION['evalprog'] = $output;
	$response = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    return $response;
}


function compute_program_area($program) {
   $lines_arr    = preg_split('/\n|\r/',$str);
   $num_newlines = count($lines_arr);
   $num_chars    = count($lines_arr[0]);
   $area         = ($num_newlines * $num_chars);
   return $num_newlines . " x " . $num_chars " = " . $area;
}

?>

<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Funge game!</title>
<link href="funge.css" rel="stylesheet" type="text/css" />

</style>
</head>
<body>
<h1 class="title">Help!</h1>
<p>I need to find a <a href="http://catseye.tc/view/befunge-93/doc/Befunge-93.markdown">Befunge-93</a> program that outputs the number 
<span class="question">
	<?php 
		if(isset($_POST['reset']) || !isset($_SESSION['quest'])) 
			$_SESSION['quest'] = random();
		unset($_SESSION['reset']); 
		echo $_SESSION['quest']; 
	?>
</span>.</p>
<p>But I also need the program's total area to be as small as possible.<br />
(Don't worry, it doesn't have to be optimal...)</p>
<p>Oh, one more thing: The commands
  <code class="emph">0-9</code>,
  <code class="emph">?</code>,
  <code class="emph">"</code>,
  <code class="emph">p</code>,
  <code class="emph">g</code>,
  <code class="emph">&amp;</code>, and
  <code class="emph">~</code>
  cannot be used.</p>
<?php  
	if(isset($_POST['submit'])) {
        $start   = microtime(true);
        $code    = __eval($_POST['program']);
        $elapsed = microtime(true) - $start;
        if( $code == 200 && $_SESSION['evalprog'] == $_SESSION['quest']) {
            printf("<p class=\"right\">Right!  :-)</p>");
            printf("<p>Program area: %s.</p>", compute_program_area('evalprog'));
            printf("<p>It took you %f seconds.</p>", $elapsed);
            printf("<form action=%s id=\"r\" name=\"r\" method=\"post\">", $_SERVER['PHP_SELF']);
            printf("<input type=\"hidden\" id=\"reset\" name=\"reset\" value=\"reset\" />");
            printf("<input type=\"submit\" name=\"again\" id=\"again\" value=\"Play again!\" />");
            printf("</form>");

        }
        else if($_SESSION['evalprog'] != $_SESSION['quest']) {
            printf("<p class=\"wrong\">Wrong!  :-(</p>");
            if($elapsed > 1) {
                printf("<p>Your program is not legal</p>");
                printf("<pre>Timeout</pre>");
            }
            else {
                printf("<p>Your program outputs:</p>");
                printf("<pre>%s </pre>", $_SESSION['evalprog']);
            }
            printf("<form action=%s id=\"r\" name=\"r\" method=\"post\">", $_SERVER['PHP_SELF']);
            printf("<input type=\"submit\" name=\"again\" id=\"again\" value=\"Try again!\" />");
            printf("</form>");
        }
    }
    else {
    		printf("<p>Enter your program that will print this number!</p>");
			printf("<form action=%s id=\"f\" name=\"f\" method=\"post\">", $_SERVER['PHP_SELF']);
 			printf("<textarea name=\"program\" id=\"program\" class=\"wide\" rows=10 cols=80></textarea>");
 			printf("<br />");
  			printf("<input type=\"submit\" name=\"submit\" id=\"submit\" value=\"Submit!\" />");
  			printf("<input type=\"submit\" name=\"reset\" id=\"reset\" value=\"Change number!\" />");
 			printf("</form>");
    }
?>

</body>
</html>

