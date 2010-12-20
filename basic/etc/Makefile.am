#SYSPART_CONF = syspart.conf

RULESET      = $(notdir $(shell cd ..; pwd))
EXTRA_DIST   = $(config_DATA)
configdir    = $(datadir)/policy/etc/$(RULESET)
config_DATA  = videoep.conf # $(SYSPART_CONF)

MAINTAINERCLEANFILES = Makefile.in
SUBDIRS = pulse ohm

install-data-hook: install-symlinks
#	mkdir -p $(DESTDIR)/$(configdir)/$(SYSPART_CONF).d

install-symlinks:
	if test "x$(INSTALL_SYMLINKS)" = "x1"; then                         \
	    if ! test -e $(DESTDIR)/$(datadir)policy/etc/current; then      \
	        ln -sf $(RULESET) $(DESTDIR)/$(datadir)/policy/etc/current; \
	    fi                                                              \
	fi
