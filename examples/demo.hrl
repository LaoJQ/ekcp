-record(state,
        {
         socket, %% just use for tcp
         echo_times = 0,
         total_rtt = 0,
         cs = s, %% c | s
         mode = tcp %% tcp | kcp
        }).

%% Command line argument
-define(SERVER_IP, "0.0.0.0").
-define(SERVER_PORT, 10001).
-define(CLIENT_NUM, 1).

-define(CLIENT_SEND_RATE, 33). %% ms
