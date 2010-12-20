RULESET      = $(notdir $(shell cd ../..; pwd))
EXTRA_DIST   = $(plugin_DATA) $(config_DATA)
plugindir    = $(datadir)/policy/etc/$(RULESET)/ohm/plugins.d
plugin_DATA  = gconf.ini accessories.ini
configdir    = $(datadir)/policy/etc/$(RULESET)/ohm
config_DATA  = ohmd.ini

MAINTAINERCLEANFILES = Makefile.in

install-data-hook: install-symlinks

install-symlinks:
	if test "x$(INSTALL_SYMLINKS)" = "x1"; then                           \
	    mkdir -p $(DESTDIR)/$(sysconfdir)/ohm;                            \
	    if ! test -e $(DESTDIR)/$(sysconfdir)/ohm/ohmd.ini; then          \
	        ln -sf $(datadir)/policy/etc/current/ohm/ohmd.ini             \
	                 $(DESTDIR)$(sysconfdir)/ohm;                         \
	    fi;                                                               \
	    mkdir -p $(DESTDIR)/$(sysconfdir)/ohm/plugins.d;                  \
	    for f in $(plugin_DATA); do                                       \
	        if ! test -e $(DESTDIR)/$(sysconfdir)/ohm/plugins.d/$$f; then \
	            ln -sf $(datadir)/policy/etc/current/ohm/plugins.d/$$f    \
                             $(DESTDIR)/$(sysconfdir)/ohm/plugins.d;          \
	        fi                                                            \
	    done                                                              \
	fi
