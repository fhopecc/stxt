import wmi
c = wmi.WMI("192.168.1.4")
for os in c.Win32_OperatingSystem():
    print os.Caption
