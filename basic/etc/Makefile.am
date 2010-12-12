#SYSPART_CONF = syspart.conf

RULESET      = $(notdir $(shell cd ..; pwd))
EXTRA_DIST   = $(config_DATA)
configdir    = $(datadir)/policy/etc/$(RULESET)
config_DATA  = videoep.conf # $(SYSPART_CONF)

MAINTAINERCLEANFILES = Makefile.in
SUBDIRS = pulse ohm

#install-data-hook:
#	mkdir -p $(DESTDIR)/$(configdir)/$(SYSPART_CONF).d
