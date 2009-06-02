-record(rabbithub_subscription, {queue, callback}).

-record(rabbithub_capability, {guid, facets, resource}).

%% RabbitMQ resource
-record(resource, {virtual_host, kind, name}).
