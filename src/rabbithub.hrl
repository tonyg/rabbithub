-record(rabbithub_subscription, {queue, callback}).

%% RabbitMQ resource
-record(resource, {virtual_host, kind, name}).
