-record(server_info, {
        id                                      :: binary(),
        ip = {0,0,0,0}  	                :: binary(),
        port = 0                         	:: integer(),
	score = 0				:: integer()
       }).

-type server_info() :: #server_info{}.
