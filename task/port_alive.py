from socket import socket
def port_alive(ip, port):
    socket.setdefaulttimeout(1)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)  
  
    result = s.connect_ex((ip, port))  
    if result == 0 :  
        return True
    s.close() 
    return False

if __name__ == "__main__":
    usage = u"usage: %prog TOP [options]"
    parser = OptionParser(usage, version="%prog 1.0")
    parser.add_option("-k", "--keyword", dest="keyword",

