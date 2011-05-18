-- 隨機抽出前 300 名轉帳繳稅
SELECT rownum,
  idn_ban
FROM
  (SELECT *
  FROM wsst130
  WHERE 1         =1
  AND trs_cd     IN ('9500019', '7011616') -- 金資轉帳帳號, 郵局轉帳帳號
  AND tax_cd      = '55'                   -- 房屋稅
  AND subtax_cd   = '1'
  AND bl_yr_pd    ='9801'
  AND coll_b_date = '0981101'
  ORDER BY dbms_random.value
  )
WHERE rownum < 300
