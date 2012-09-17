PACKAGE_NAME:=rabbithub
APP_NAME:=rabbithub
DEPS:=rabbitmq-server rabbitmq-erlang-client rabbitmq-mochiweb

construct_app_commands=\
	cp -rp priv $(APP_DIR)/.
