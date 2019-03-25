-record(server_info, {
        id                                      :: binary(),
        ip = {0,0,0,0}  	                :: binary(),
        port = 0                         	:: integer(),
	score = 0				:: integer()
       }).

-type server_info() :: #server_info{}.

-define(OP_IDENT, 1:16).
-define(OP_CHALLENGE, 2:16).
-define(OP_PROOF, 3:16).
-define(OP_PROOF_ACK, 4:16).
-define(OP_PROOF_ACK_ACK, 5:16).
-define(OP_AUTH_CMP, 6:16).
-define(OP_AUTH_CMP_ACK, 7:16).

