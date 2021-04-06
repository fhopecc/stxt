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

@task
def markdown(c):
    '要轉檔的markdown檔'
    import markdown
    from pathlib import Path
    m = Path(r'C:\\') / 'gd' / '110農地專調' / '納管輔導金.txt'
    with m.open('r', encoding='utf8') as f:
        html = markdown.markdown(f.read())
        print(html)
