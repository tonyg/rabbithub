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
