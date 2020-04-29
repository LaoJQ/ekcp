-ifndef(DEFINE_EKCP_HRL).
-define(DEFINE_EKCP_HRL, true).

-record(ekcp_conf,
        {
         %% ekcp_nodelay
         nodelay = 0, %% 0 | 1 | 2,
         interval = 100, %%  10...5000,
         resend = 0, %% integer(),
         nc = 0, %% 0 | 1,

         %% ekcp_wndsize
         sndwnd = 32,
         rcvwnd = 128,

         %% setmtu
         mtu = 1400,

         %% Either flush immediately after send or not.
         send_nodelay = false,

         %% Set a threshold value of waitsnd, when lager than it, close this kcp connect.
         snd_queue_threshold = 64
        }).

-define(EKCP_HEADER_SIZE, 24).

-define(EKCP_SEND_EAGAIN, -10).

-endif.
