// php 7.1
<?php
session_start();
function eval_bitcoin($bitcoin)
{
    return hash('SHA256', hex2bin(hash('SHA256', hex2bin($bitcoin))));
}

function check_magic_code($hashed, $magic)
{
    $magic_from_hash = substr($hashed, 0, 4);
    if ($magic_from_hash == $magic) {
        // echo hexdec(substr($hashed, 4, 4)) / 100.0;
        return hexdec(substr($hashed, 4, 4)) / 100.0;
    } else
        return -1;
}

function generateRandomString($length)
{
    $characters = '0123456789abcdef';
    $charactersLength = strlen($characters);
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, $charactersLength - 1)];
    }
    return $randomString;
}

?>
<!DOCTYPE html PUBLIC
        "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <title>Gimme a bitcoin!</title>
    <style type="text/css">
        <!--
        body, td, th {
            font-family: Verdana, Arial, Helvetica, sans-serif;
            font-size: x-large;
            color: #CCCCCC;
        }

        body {
            background-color: #333399;
        }

        .title {
            font-family: "Courier New", Courier, monospace;
            font-weight: bold;
            font-size: 48px;
            color: #00FF66;
        }

        .question {
            color: #FFCC33
        }

        .number {
            color: #FFFF33
        }

        .md5sum {
            color: #FFCCFF
        }

        .emph {
            color: #99ee99
        }

        .alert {
            color: #ee77aa
        }

        .right {
            color: #33FF66;
            font-weight: bold;
        }

        .wrong {
            color: #FF3366;
            font-weight: bold;
        }

        a:link {
            color: #CCFFFF;
        }

        a:visited {
            color: #CCFFFF;
        }

        input {
            background-color: #eeee66;
            color: #333399;
        }

        code {
            text-wrap: lowercase;
            font-family: monospace;
            display: block;
            background-color: #66eeee;
            color: #993333;
            border: 1px solid black;
            padding: 8px;
            width: 95%;
            margin-top: 0.25em;
            margin-bottom: 0.25em;
        }

        input.wide {
            text-wrap: lowercase;
            font-family: monospace;
            font-size: x-large;
            color: #333333;
            border: 1px solid black;
            padding: 8px;
            width: 95%;
        }

        -->
    </style>
</head>
<body>
<h1>Gimme a bitcoin!</h1>

<blockquote>
    <p>For the purpose of this exercise, <span class="emph">bitcoins</span>
        are 256-bit hexadecimal numbers, which, when hashed twice using SHA256,
        start with the 16-bit <span class="emph">magic code</span> given on this
        page. Notice that the magic code frequently changes.
    </p>

    <p>The 16-bits immediately after the magic code represent the bitcoin's
        <span class="emph">value</span>, given in euro cents.
    </p>

    <p>Bitcoins are represented in hexadecimal form, as strings of 64
        hexadecimal digits.<br/>
        Magic codes are represented as strings of 4 hexadecimal digits.
    </p>

    <p><span class="alert">Example:</span> If the magic code is 4217,
        the following string is a bitcoin worth 7.99 euro:
        <code>796fae438ebdc83ac3a4e8a071d71b1f0f0eace40d8a5b92bb64b1e9ed746066</code>
    </p>
</blockquote>


<?php
if (!isset($_SESSION['quest']))
    $_SESSION['quest'] = generateRandomString(4);

if (!isset($_SESSION['owed']))
    $_SESSION['owed'] = 2000;

if (isset($_POST['submit'])) {
    $value = check_magic_code(eval_bitcoin($_POST['answer']), $_SESSION['quest']);
    if ($value != -1) {
        $_SESSION['owed'] -= $value;

        if($_SESSION['owed'] > 0) {
            echo
                "<p> I'd like to have 2000.00 euros, you still owe me " . $_SESSION['owed'] . " </p>";
            echo "
              <p class=\"right\">Right!  :) </p>
              <hr>
              <form action=\"/gimmeabitcoin.php\" id=\"r\" name=\"r\" method=\"post\">
                    <input type=\"hidden\" id=\"continue\" name=\"continue\" value=\"continue\">";
            echo "<input type=\"submit\" name=\"again\" id=\"again\" value=\"Continue!\"></form>";
        }
        else {
            echo
                "<p> You have paid your depts and maybe more...</p>";
            echo "
              <p class=\"right\">Right!  :) </p>
              <hr>
              <form action=\"/gimmeabitcoin.php\" id=\"r\" name=\"r\" method=\"post\">
                    <input type=\"hidden\" id=\"continue\" name=\"continue\" value=\"continue\">";
            echo "<input type=\"submit\" name=\"again\" id=\"again\" value=\"Play again!\"></form>";
            unset($_SESSION['owed']);
        }
    } else {
        echo
            " <p> I'd like to have 2000.00 euros, you still owe me " . $_SESSION['owed'] . " </p>
          <p class=\"wrong\"> This is not a valid bitcoin! :-( </p>
		  <hr>
		  <form action=\"/gimmeabitcoin.php\" id=\"r\" name=\"r\" method=\"post\">
				<input type=\"hidden\" id=\"continue\" name=\"continue\" value=\"continue\">
				<input type=\"submit\" name=\"again\" id=\"again\" value=\"Continue!\">
		  </form>";
        unset($_SESSION['owed']);
    }
    unset($_SESSION['quest']);
} else {
    unset($_POST['submit']);
    if (isset($_POST['reset']))
        $_SESSION['quest'] = generateRandomString(4);
    unset($_POST['reset']);

    echo " <p> I'd like to have 2000.00 euros, you still owe me " . $_SESSION['owed'] . "</p> ";

    echo "<span class=\"question\"> The magic code is " . $_SESSION['quest'] . " </span> </p>";

    echo "<form action=\"/gimmeabitcoin.php\" id=\"f\" name=\"f\" method=\"post\">
            <input type=\"text\" class=\"wide\" name=\"answer\" id=\"answer\" /><br />
            <input type=\"submit\" name=\"submit\" id=\"submit\" value=\"Submit!\" />
            <input type=\"reset\" value=\"Reset\" />
          </form> ";
}
?>
</body>
</html>

