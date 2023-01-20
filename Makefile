all:
	rm -rf  *~ */*~ src/*.beam tests/*.beam tests_ebin erl_cra*;
	rm -rf logs log *.pod_dir;
	rm -rf _build tests_ebin ebin;
	rm -f rebar.lock;
#	tests 
	mkdir tests_ebin;
	erlc -I include -o tests_ebin tests/*.erl;
	rm -rf tests_ebin;
#  	dependencies
	mkdir ebin;
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
build:
	rm -rf  *~ */*~ src/*.beam tests/*.beam tests_ebin erl_cra*;
	rm -rf logs log *.pod_dir;
	rm -rf _build tests_ebin ebin;
	rm -f rebar.lock;
#	tests 
	mkdir tests_ebin;
	erlc -I include -o tests_ebin tests/*.erl;
	rm -rf tests_ebin;
#  	dependencies
	mkdir ebin;
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
clean:
	rm -rf  *~ */*~ src/*.beam tests/*.beam tests_ebin erl_cra*;
	rm -rf logs log *.pod_dir;
	rm -rf _build tests_ebin ebin;
	rm -f rebar.lock;

eunit:
	rm -rf  *~ */*~ src/*.beam tests/*.beam tests_ebin erl_cra*;
	rm -rf logs log *.pod_dir;
	rm -rf _build tests_ebin ebin;
	rm -rf *.dir;
	rm -f rebar.lock;
#	tests 
	mkdir tests_ebin;
	erlc -I include -o tests_ebin tests/*.erl;
#  	dependencies
#	Applications
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	erl -pa */ebin -pa ebin -pa tests_ebin -sname do_test -run $(m) start $(a) $(b) -setcookie cookie_test
