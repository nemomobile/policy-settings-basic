dist_ruleset_SCRIPTS = policy.plc
RULESET      = $(notdir $(shell cd ..; pwd))
EXTRA_DIST   = $(ruleset_DATA)
rulesetdir   = $(datadir)/policy/rules/$(RULESET)
ruleset_DATA = policy.dresc $(SOURCES)
SOURCES      = $(shell ls *.pl *.dres)
LIBPROLOG    = /usr/share/libprolog/libprolog.pl

DRESC = @DRESC@
PLC   = @PLC@

MAINTAINERCLEANFILES = Makefile.in

policy.dresc: policy.dres
	$(DRESC) $< -o $@

policy.plc: policy.pl $(LIBPROLOG)
	$(PLC) -q -t precompile -s ../../scripts/plc -- -c $? -o $@

clean-local:
	rm -f *~ *.plc *.dresc

install-data-hook: install-symlinks

install-symlinks:
	if test "x$(INSTALL_SYMLINKS)" = "x1"; then                           \
	    if ! test -e $(DESTDIR)/$(datadir)/policy/rules/current; then     \
	        ln -sf $(RULESET) $(DESTDIR)/$(datadir)/policy/rules/current; \
	    fi                                                                \
	fi
