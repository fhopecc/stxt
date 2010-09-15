/**--------------------------
//* Validate Date Field script- By JavaScriptKit.com
//* For this script and 100s more, visit http://www.javascriptkit.com
//* This notice must stay intact for usage
---------------------------**/

function validate_email(field,alerttxt)
{
with (field)
  {
  apos=value.indexOf("@");
  dotpos=value.lastIndexOf(".");
  if (apos<1||dotpos-apos<2)
    {alert(alerttxt);return false;}
  else {return true;}
  }
}
