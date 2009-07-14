MARKDOWN_SOURCES=$(wildcard doc/*.md)
MARKDOWN_TARGETS=$(patsubst doc/%.md,doc/html/%.html,$(MARKDOWN_SOURCES))

all: ebin
	(cd src;$(MAKE))

docs: html-docs

html-docs: doc/html $(MARKDOWN_TARGETS)

doc/html:
	mkdir -p doc/html

doc/html/%.html: doc/%.md
	(title=`grep '^# ' $< | head -1 | sed -e 's:^# ::'` ;\
	 t=/tmp/$*.md ;\
	 sed -e "s:@TITLE@:$$title:g" < doc/header.html > $@ ;\
	 python doc/buildtoc.py < $< > $$t ;\
	 markdown $$t >> $@ ;\
	 rm $$t ;\
	 cat doc/footer.html >> $@)

ebin:
	mkdir -p ebin

clean: clean-docs
	(cd src;$(MAKE) clean)

clean-docs: clean-html

clean-html:
	rm -rf doc/html

distclean: clean

install: all
	@[ -n "$(DESTDIR)" ] || (echo "Please set DESTDIR."; false)
	@[ -n "$(TARGET_DIR)" ] || (echo "Please set TARGET_DIR."; false)
	@[ -n "$(SBIN_DIR)" ] || (echo "Please set SBIN_DIR."; false)
	@[ -n "$(MAN_DIR)" ] || (echo "Please set MAN_DIR."; false)

	mkdir -p $(DESTDIR)/$(TARGET_DIR)
	cp -r ebin deps priv README.md $(DESTDIR)/$(TARGET_DIR)

	chmod 0755 scripts/*
	mkdir -p $(DESTDIR)/$(SBIN_DIR)
	sed -e "s:@TARGET_DIR@:${TARGET_DIR}:g" scripts/rabbithub.in \
		> $(DESTDIR)/$(SBIN_DIR)/rabbithub
	chmod 0755 $(DESTDIR)/$(SBIN_DIR)/rabbithub

# 	for section in 1; do \
# 		mkdir -p $(DESTDIR)/$(MAN_DIR)/man$$section; \
# 		for manpage in docs/*.$$section.pod; do \
# 			cp docs/`basename $$manpage .pod`.gz $(DESTDIR)/$(MAN_DIR)/man$$section; \
# 		done; \
# 	done
