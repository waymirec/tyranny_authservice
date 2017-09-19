-record(game_server, {
        id                                      :: binary(),
        int_ip					:: binary(),
        ext_ip                                  :: binary(),
        ext_port                                :: integer()
       }).

-type game_server() :: #game_server{}.
