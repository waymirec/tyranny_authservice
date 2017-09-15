-module(inet_util).

-export([
         ip_to_int/1,
         int_to_ip/1
        ]).

ip_to_int({A,B,C,D} = _IpAddress) -> (A*16777216)+(B*65536)+(C*256)+(D).
int_to_ip(IpAddr) -> {IpAddr bsr 24, (IpAddr band 16711680) bsr 16, (IpAddr band 65280) bsr 8, IpAddr band 255}.
