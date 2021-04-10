all:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf gen_mnesia/ebin/*;
	rm -rf  *~ */*~  erl_cra*;
#	gen_mnesia
	cp /home/joq62/erlang/simple_erlang/server_nodes/gen_mnesia/src/*.app gen_mnesia/ebin;
	erlc -o gen_mnesia/ebin  /home/joq62/erlang/simple_erlang/server_nodes/gen_mnesia/src/*.erl;
#	compute
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa\
	    -pa */ebin\
	    -setcookie abc\
	    -compute config_file nodes\
	    -sname compute\
	    -mnesia dir mneisa_dir\
	    -run compute boot

monkey:
	erlc -o test_ebin test_src/monkey_test.erl;
	erl -pa test_ebin\
	    -setcookie abc\
	    -sname monkey_test\
	    -run monkey_test start
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf gen_mnesia/ebin/*;
	rm -rf  *~ */*~  erl_cra*;
#	gen_mnesia
	cp /home/joq62/erlang/simple_erlang/server_nodes/gen_mnesia/src/*.app gen_mnesia/ebin;
	erlc -o gen_mnesia/ebin  /home/joq62/erlang/simple_erlang/server_nodes/gen_mnesia/src/*.erl;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
#	test application
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -pa */ebin\
	    -setcookie abc\
	    -compute config_file nodes\
	    -sname compute\
	    -mnesia dir mneisa_dir\
	    -run compute_unit_test test
test:
	rm -rf db*/* ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
#	Common service
	erlc -o ebin ../../services/common_src/src/*.erl;
	erlc -o db0 ../../services/common_src/src/*.erl;
	erlc -o db1 ../../services/common_src/src/*.erl;
	erlc -o db2 ../../services/common_src/src/*.erl;
#	Common log
	erlc -o ebin ../../services/log_src/src/*.erl;
#	service
	cp test_src/dbase.app db0;
	cp test_src/dbase.app db1;
	cp test_src/dbase.app db2;
	erlc -o db0 src/*.erl;
	erlc -o db1 src/*.erl;
	erlc -o db2 src/*.erl;
	erlc -o db0 test_src/*.erl;
	erlc -o db1 test_src/*.erl;
	erlc -o db2 test_src/*.erl;
	erlc -o ebin src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -setcookie abc\
	    -sname test_dbase\
	    -run  dbase_unit_test start_test test_src/test.config
stop:
	erl_call -a 'rpc call [master@c0 init stop []]' -sname master -c abc;
	erl_call -a 'rpc call [master@c1 init stop []]' -sname master -c abc;
	erl_call -a 'rpc call [master@c2 init stop []]' -sname master -c abc
