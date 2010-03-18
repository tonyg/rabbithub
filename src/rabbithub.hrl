-record(rabbithub_subscription, {resource, topic, callback}).

-record(rabbithub_lease, {subscription,
                          lease_duration_seconds,
                          lease_expiry_time_microsec,
                          secret,
                          token}).
-record(rabbithub_subscription_pid, {subscription, pid, expiry_timer}).
