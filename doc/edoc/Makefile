all:
	$(MAKE) edoc

clean:
	rm -f *.html *.png *.css edoc-info

edoc:
	cp ../../module/*.erl .
	erl -noshell -run edoc_run files [comm.erl,dvh.erl,mrh.erl,mvh.erl,rih.erl,driver.erl,memo.erl,multi.erl,player.erl,trimaths.erl]
	rm *.erl
