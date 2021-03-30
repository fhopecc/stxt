from invoke import task

@task
def backup_hlao(c):
    'TODO:備份辦公室 hlao 至隨身碟'
    print('TODO:備份辦公室 hlao 至隨身碟')

@task
def example(c, name):
    '範例'
    c.run(f"echo {name}加油!")
