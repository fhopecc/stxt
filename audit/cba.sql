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
