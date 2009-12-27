# -*- coding: UTF-8 -*-
import ldap

def ldap_login(username, password):
    Server = "ldap://rdc.hltb.gov.tw"
    DN = "dc=hltb,dc=gov,dc=tw"

    l = ldap.initialize(Server)
    l.set_option(ldap.OPT_REFERRALS, 0) # 使驗證不需 administrator
    l.protocol_version = 3
    try:
        l.simple_bind_s(username, password) # (同步) bind 成功即為通過驗證

        Base = "dc=hltb,dc=gov,dc=tw"
        Scope = ldap.SCOPE_SUBTREE
        Filter = "(sAMAccountName="+ username +")"
        #Attrs = ["displayName","givenName"]
        Attrs = None

        r = l.search(Base, Scope, Filter, Attrs)
        Type, user = l.result(r,0)

        # 回傳中文名稱 (utf8)
        return (user[0][1]['givenName'][0], user[0][1]['title'][0])

    except ldap.LDAPError, e:     # 若 bind 失敗，即丟例外
        print e    # 印出錯誤訊息
        return 

if __name__ == "__main__":
    r = ldap_login("ccl00695", "btmw111$")
    if r:
        print r[0].decode('utf8').encode('cp950'),
        print "Login!"
    else:
        print u"登入失敗!", 
        print "NOT login!"
