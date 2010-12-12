RULESET      = $(notdir $(shell cd ../..; pwd))
EXTRA_DIST   = $(plugin_DATA) $(config_DATA)
plugindir    = $(datadir)/policy/etc/$(RULESET)/ohm/plugins.d
plugin_DATA  = gconf.ini accessories.ini
configdir    = $(datadir)/policy/etc/$(RULESET)/ohm
config_DATA  = ohmd.ini

MAINTAINERCLEANFILES = Makefile.in

