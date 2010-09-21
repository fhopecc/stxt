function check_date(field, msg) {
    with(field) {
        var pat = /(\d{4})(\d{2})(\d{2})/
        var m = pat.exec(value)
        if (m != null){
          year = m[1]
          mon  = m[2]
          day  = m[3]
          return true
        } else {
          alert(msg)
          focus()
          return false
        }
    }
}

function check_email(field,msg)
{
with (field)
  {
  apos=value.indexOf("@");
  dotpos=value.lastIndexOf(".");
  if (apos<1||dotpos-apos<2)
    {alert(msg);focus();return false;}
  else {return true;}
  }
}

// 身份證字號
function checkID(string) {
  re = /^[A-Z]\d{9}$/;
  if (re.test(string))
   return true; 
 }
 
// 信用卡號
function checkCreditCard(string) {
  re = /^\d{4}-\d{4}-\d{4}-\d{4}$/;
 // re = /^(\d{4}-){3}\d{4}$/;
  if (re.test(string))
   return true; 
 }
 
// 英文名稱
function checkEnglishName(string) {
  re1 = /^[A-Za-z\-]+\s+[A-Za-z\-]+$/;
  re2 = /^[A-Za-z\-]+\s+[A-Za-z\-]+\s+[A-Za-z\-]+$/;
  if (re1.test(string) || re2.test(string))
   return true; 
 }
 
// 電話
function check_phone(field, msg)
{
    re = /^[\d()-]+$/;
    with (field) {
        if (re.test(value)) {
            return true; 
        } else {
            alert(msg)
            focus()
            return false
        }
    }
}

// 價格
function check_integer(field, msg)
{
    re = /^[\d]+$/;
    with (field) {
        if (re.test(value)) {
            return true; 
        } else {
            alert(msg)
            focus()
            return false
        }
    }
}
