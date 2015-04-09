to_stop_node="gsvr@127.0.0.1"
cookie="gsvr"
erl -pa ebin -name stop@127.0.0.1 -setcookie $cookie -s gsvr stop_node $to_stop_node
