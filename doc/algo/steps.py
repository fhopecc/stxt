# coding=utf-8
def steps(n):
    if (n == 0 or n == 1):
        return 1 
    else:
        return steps(n-1) + steps(n-2);

table = {} # 表格，儲存全部問題的答案。 
solve = {} # 記錄問題是否已計算 

def steps_dp1(n):
    if n == 0 or n == 1:
        return 1
    try:
        if solve[n]:
            return table[n]; 
    except:
        pass

    table[n] = steps_dp1(n-1) + steps_dp1(n-2) # 將答案存入表格 
    solve[n] = True;  # 紀錄已計算 
    return table[n]; 


def run():
    print steps(35)

def run2():
    print steps_dp1(100)

run2()
#import cProfile
#cProfile.run('run()')
#cProfile.run('run2()')
