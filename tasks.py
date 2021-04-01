from invoke import task

@task
def backup_hlao(c):
    '備份辦公室 hlao 至隨身碟'
    '運用 backupy 備份資料'
    'backupy <source> <dest> [options]'
    'TODO:來源刪除的檔，同步刪除備份源'
    'TODO:彩色顯示'
    import os
    os.system('backupy d:\hlao g:\hlao')

@task
def example(c, name):
    '範例'
    c.run(f"echo {name}加油!")
