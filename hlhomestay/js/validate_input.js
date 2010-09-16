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


// Checks if a given date string is in    
// one of the valid formats:   
// a) M/D/YYYY format   
// b) M-D-YYYY format   
// c) M.D.YYYY format   
// d) M_D_YYYY format   
function isDate(s)   
{      
    // make sure it is in the expected format   
    if (s.search(/^\d{1,2}[\/|\-|\.|_]\d{1,2}[\/|\-|\.|_]\d{4}/g) != 0)   
        return false;   
  
    // remove other separators that are not valid with the Date class              
    s = s.replace(/[\-|\.|_]/g, "/");   
               
    // convert it into a date instance   
    var dt = new Date(Date.parse(s));          
  
    // check the components of the date   
    // since Date instance automatically rolls over each component   
    var arrDateParts = s.split("/");   
    return (   
        dt.getMonth() == arrDateParts[0]-1 &&   
        dt.getDate() == arrDateParts[1] &&   
        dt.getFullYear() == arrDateParts[2]   
    );             
}   
  
// test function to test the isDate function   
function test_isDate()   
{   
    var arrDates = [    
        '05/15/2008', '05-15-2008',    
        '05.15.2008', '05_15_2008',   
        '15/15/2008', '05/35/2008',   
        '05/15/1908', '15-15-2008',   
        'a/1b/2008', '05/30/a008' ];   
           
    for (var d in arrDates)    
        document.writeln("isDate('" + arrDates[d] + "') : " + isDate(arrDates[d]) + "<BR>");   
}
