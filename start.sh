node="gsvr@127.0.0.1"
cookie="gsvr"
erl -pa ebin -pa deps/*/ebin -name $node -setcookie $cookie -config gsvr.config -s gsvr
