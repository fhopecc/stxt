
$def with (r)
$code:
    h = r.homestay
<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01//EN' 'http://www.w3.org/tr/html4/strict.dtd'>
<html lang="zh-tw">
<head><title>新增客房</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
</head>
<body>
<h1>民宿資料</h1>
<table>
<tr><td>名稱<td>$:h.name</td></tr>
<tr><td>地址</td><td>$:h.address</td></tr>
<tr><td>主人</td><td>$:h.owner</td></tr>
<tr><td>電子郵件</td><td>$:h.email</td></tr>
<tr><td>部落格</td><td>$:h.blog</td></tr>
</table>
<h1>客房資料</h1>
<form method="post" action="/rooms/$:r.key()/edit">
  <table>
    <tr><td>名稱</td>
      <td><input name="name" type="text" value="$:r.name"/></td>
    </tr>
    <tr><td>平日價格</td>
      <td><input name="price" type="text" value="$:r.price"/></td>
    </tr>
    <tr><td>假日價格</td>
      <td><input name="holiday_price" type="text"
          value="$:r.holiday_price"/></td>
    </tr>
    <tr><td><input type="submit" value="更新"/></td></tr>
  </table>
</form>
</body>
</html>
