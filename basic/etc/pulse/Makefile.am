RULESET      = $(notdir $(shell cd ../..; pwd))
EXTRA_DIST   = $(config_DATA)
configdir    = $(datadir)/policy/etc/$(RULESET)/pulse
config_DATA  = xpolicy.conf

MAINTAINERCLEANFILES = Makefile.in

install-data-hook: install-symlinks

install-symlinks:
	if test "x$(INSTALL_SYMLINKS)" = "x1"; then                        \
	    mkdir -p $(DESTDIR)/$(sysconfdir)/pulse;                       \
	    if ! test -e $(DESTDIR)/$(sysconfdir)/pulse/xpolicy.conf; then \
	        ln -sf $(datadir)/policy/etc/current/pulse/xpolicy.conf    \
                       $(DESTDIR)/$(sysconfdir)/pulse/xpolicy.conf;        \
	    fi                                                             \
	fi
