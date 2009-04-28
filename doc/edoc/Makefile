all:
	$(MAKE) edoc

clean:
	rm [^MD]*

edoc:
	cp ../../module/*.erl .
	erl -noshell -run edoc_run files [comm.erl,dvh.erl,mrh.erl,mvh.erl,rih.erl,driver.erl,memo.erl,multi.erl,player.erl]
	rm *.erl
