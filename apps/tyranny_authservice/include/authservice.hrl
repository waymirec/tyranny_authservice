-record(game_server, {
        id                                      :: binary(),
        int_ip = {0,0,0,0}			:: binary(),
	int_port = 0				:: binary(),
        ext_ip = {0,0,0,0}                      :: binary(),
        ext_port = 0                            :: integer(),
	score = 0				:: integer()
       }).

-type game_server() :: #game_server{}.

-record(server_info, {
        id = <<"">>                             :: binary(),
        int_ip = <<"">>                         :: binary(),
        int_port = 0                            :: integer(),
        ext_ip = <<"">>                         :: binary(),
        ext_port = 0                            :: integer(),
        num_conns = 0                           :: integer(),
        sys_load_last = 100                     :: integer(),
        sys_load_1min = 100                     :: integer(),
        sys_load_5min = 100                     :: integer(),
        sys_load_15min = 100                    :: integer(),
        mem_total = 0                           :: integer(),
        mem_free = 0                            :: integer()
       }).

-type server_info() :: #server_info{}.
