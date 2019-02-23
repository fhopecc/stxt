from aisimporter import import_xls1
from aisimporter import import_dir_xls
from aisimporter import drop_table
from aisimporter import chinese_word_count
#import_dir_xls('縣府提供資訊檔案/長照核銷紀錄', '長照核銷二', 'long_care.db', start_row=1, end_row_str='總計')


#import_dir_xls('縣府提供資訊檔案/送餐/麥子'
#              ,'麥子送餐核銷', 'long_care.db' 
#              ,start_row=1 
#              ,end_row_str=''
#              )

chinese_word_count("縣長信箱及1999專線資料分析 - 工作表4.csv","案件詞數表2.csv")
