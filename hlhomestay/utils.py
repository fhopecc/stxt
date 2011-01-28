def get_account(path):
    pat = r'/homestays/(\w+)'
    m = re.match(pat, path)
    return m.group(1)
