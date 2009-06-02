-record(rabpubsubhub_subscription, {queue, callback}).

-record(rabpubsubhub_capability, {guid, facets, resource}).

%% RabbitMQ resource
-record(resource, {virtual_host, kind, name}).
