def index(mes, resp):
    response.out.write('list users')

def show(id, mes, resp):
    return "list users %d" % id

def destroy(id):
    return "del test %d" % id
