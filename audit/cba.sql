-- cba001.總分類帳分析-借貸不平衡之傳票編號
-- 20180614 張簡稜剛
with d as (select 傳票編號, sum(金額) as 借方金額
           from 總分類帳
           where 借貸 = '借'
           group by 傳票編號), 
     c as (select 傳票編號, sum(金額) as 貸方金額
           from 總分類帳
           where 借貸 = '貸'
           group by 傳票編號)
select d.傳票編號
from d join c on d.傳票編號 = c.傳票編號
where d.借方金額 - c.貸方金額 <> 0
-- cba002.摘要含車號之紀錄
-- 20180614 張簡稜剛
select *
from 總分類帳
where 摘要 regexp "\d+-[A-Z]+|[A-Z]+-\d+" 
and 借貸='借'
and 會計分類='經費類'
-- cba003.總帳計算淨資產餘額
-- 20180618 張簡稜剛
with c as (
select sum(金額) 貸方金額
from 總分類帳
where 科目名稱='資產負債淨額-資產負債淨額'
and 借貸 = '貸'), d as(
select sum(金額) 借方金額
from 總分類帳
where 科目名稱='資產負債淨額-資產負債淨額'
and 借貸 = '借')
select 貸方金額 - 借方金額 as 淨資產餘額
from c, d 
-- cba004.以受款人名稱查調小額採購明細清單
-- 20180627 張簡稜剛
select b.受款人名稱, a.傳票日期, a.傳票類型, c.預算科目名稱 , d.用途別一級, d.用途別二級, d.用途別三級, a.科目名稱, a.摘要, a.金額, a.借貸
from 總分類帳 a, 發票檔 b, 預算明細分類帳 c, 付款憑單 d
where a.傳票編號 = b.傳票編號
and b.傳票編號 = c.傳票編號
and a.傳票編號 = d.傳票編號
and a.金額 = b.發票金額
and a.金額 = c.金額
and a.金額 = d.金額
and b.受款人名稱 in ('宏基水電行麥高智', '泳碩有限公司', '常榮機械股份有限公司', '億霖企業行楊文邦')
and a.借貸 = '借'
order by b.受款人名稱, a.傳票日期
-- cba004.串接各檔
select a.*, b.*, c.*
from 總分類帳 a, 發票檔 b, 付款憑單 c
where a.傳票編號='900251070500105'
and a.傳票編號 = b.傳票編號
and a.傳票編號 = c.傳票編號
-- 調閱符合某項金額之所有傳票
select 科目名稱, 傳票日期, 傳票類型, 傳票編號, 摘要, 金額, 借貸
from 總分類帳
where 傳票編號 in (select 傳票編號 from 總分類帳 where 金額 in (select 金額 from 總分類帳 where 摘要 like '%紀惠美%'))
order by 傳票編號
--發票日期與付款憑單編製日期相距日數
select a.科目名稱, a.傳票日期, a.傳票編號, a.傳票類型 , a.摘要, a.金額, b.營利事業統一編號, b.受款人名稱,  b.發票號碼, b.發票日期,  b.發票金額, 
julianday(substr(a.傳票日期+19110000, 1, 4)||'-'||substr(a.傳票日期+19110000,5,2)||'-'||substr(a.傳票日期+19110000,7,2))- julianday(substr(b.發票日期+19110000, 1, 4)||'-'||substr(b.發票日期+19110000,5,2)||'-'||substr(b.發票日期+19110000,7,2))
相距日數
from 總分類帳 a, 發票檔 b
where 1=1
and a.傳票編號 = b.傳票編號
and a.借貸= '借'
and 相距日數 > 30
order by 金額
--特定受款人總帳
select *
from 總分類帳
where 傳票編號 in
(select 傳票編號
from 受款人檔
where 受款人名稱='花蓮縣家庭教育中心')
--符合某項條件的傳票，該傳票的完整紀錄
select *
from 總分類帳
where 傳票編號 in(
select 傳票編號
from 總分類帳
where 摘要 like '%我和我的%')
order by 傳票編號
--符合某項條件的傳票受款人資訊
select a.傳票編號, a.科目名稱, a.傳票日期, a.傳票類型, a.摘要,
a.金額, b.受款人名稱, b.金融機構名稱, b.戶名,
b.`(國庫)支票號碼`, b.單據類別,  b.支付金額
from 總分類帳 a, 受款人檔 b
where a.傳票編號 in(
select 傳票編號
from 總分類帳
where 摘要 like '%我和我的%')
and a.傳票編號 = b.傳票編號
and a.金額=b.支付金額
