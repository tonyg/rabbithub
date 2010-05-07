PACKAGE=rabbithub
DEPS=rabbitmq-server rabbitmq-erlang-client rabbitmq-mochiweb
RUNTIME_DEPS=rabbitmq-mochiweb
EXTRA_PACKAGE_DIRS=priv
include ../include.mk

MARKDOWN_SOURCES=$(wildcard doc/*.md)
MARKDOWN_TARGETS=$(patsubst doc/%.md,doc/html/%.html,$(MARKDOWN_SOURCES))

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

clean:: clean-docs

clean-docs: clean-html

clean-html:
	rm -rf doc/html
