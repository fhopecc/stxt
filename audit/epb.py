from aisimporter import import_xls1
from aisimporter import import_dir_xls
from aisimporter import drop_table
from aisimporter import chinese_word_count
from aisimporter import import_csv2
#import_dir_xls('縣府提供資訊檔案/長照核銷紀錄', '長照核銷二', 'long_care.db', start_row=1, end_row_str='總計')


#import_dir_xls('縣府提供資訊檔案/送餐/麥子'
#              ,'麥子送餐核銷', 'long_care.db' 
#              ,start_row=1 
#              ,end_row_str=''
#              )

#chinese_word_count("縣長信箱及1999專線資料分析 - 工作表4.csv","案件詞數表2.csv")

#import_dir_xls('廢食用油勾稽產源清冊查無營運紀錄', '廢食用油產源無營運紀錄', 'epb.db')

import_csv2('D:\gdrive\環保基金財抽\廢食用油\檢具廢清書情形\未按規定申報廢棄書\花蓮商登.csv', 
            '花蓮商業登記', 
            'D:\gdrive\環保基金財抽\廢食用油\檢具廢清書情形\未按規定申報廢棄書\data.db')
