Summary: Poor man's remote syslog message sorter
Name: rxlogd
Version: 0.10
Release: 1
License: LGPL
Buildroot: %{_tmppath}/%{name}-buildroot
Group: System Environment/Base
Source: %{name}.tar.gz
Requires: coreutils initscripts chkconfig python >= 2.3
BuildArch: noarch

%description
Rxlogd will listen on a socket, receive syslog messages, and write them to
files separated per hostname. It does not disturb the local syslogd.
A built-in DNS cache reduces DNS lookup load. Logfiles can be rotated with
the supplied example logrotate configuration.

%prep
%setup -n %{name}

%build

%install
mkdir -p %{buildroot}/usr/bin
mkdir -p %{buildroot}/etc/sysconfig
mkdir -p %{buildroot}/etc/logrotate.d
mkdir -p %{buildroot}/etc/init.d
mkdir -p %{buildroot}/etc/cron.hourly
install -m 644 -o root -g root sysconfig %{buildroot}/etc/sysconfig/rxlogd
install -m 755 -o root -g root initd %{buildroot}/etc/init.d/rxlogd
install -m 755 -o root -g root logrotate %{buildroot}/etc/logrotate.d/rxlogd
install -m 755 -o root -g root rxlogd %{buildroot}/usr/bin/
install -m 755 -o root -g root cron %{buildroot}/etc/cron.hourly/rxlogd

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
/usr/bin/rxlogd
%config /etc/init.d/rxlogd
%config /etc/sysconfig/rxlogd
%config /etc/logrotate.d/rxlogd
%config /etc/cron.hourly/rxlogd

%post
/sbin/chkconfig rxlogd on
/sbin/service rxlogd restart

%changelog
* Fri Oct 24 2008 Andras Horvath <Andras.Horvath@cern.ch>	0.10-1
- added automatic newline support (for rsyslog clients)

* Fri Oct 10 2008 Andras Horvath <Andras.Horvath@cern.ch>	0.9-1
- fix bug where IPs were not properly used if DNS resolution fails

* Tue Sep 02 2008 Andras Horvath <Andras.Horvath@cern.ch>   0.8-1
- close logfiles after a period of inactivity (and free memory)

* Tue Apr 22 2008 Andras Horvath <Andras.Horvath@cern.ch>   0.7-1
- don't print "[OK]" when reopening logs (init script fix)

* Thu Feb 07 2008 Andras Horvath <Andras.Horvath@gmail.com>	0.6-1
- fixed "exit upon DNS resolution problem" bug
- postinstall/preuninstall scripts' fix

* Wed Nov 14 2007 Andras Horvath <Andras.Horvath@gmail.com>	0.3-1
- cosmetic fix for .tar.gz
- debug levels now cleaner, errors logged by default, no stdout

* Mon Jul 23 2007 Andras Horvath <Andras.Horvath@gmail.com>	0.2-4
- logrotate now puts old logs in subdirectory
- cosmetic changes to init script

* Sun Apr 01 2007 Andras Horvath <Andras.Horvath@gmail.com>
- updated contact info

* Tue Mar 28 2007 Andras Horvath <Andras.Horvath@gmail.com>
- first attempt 
