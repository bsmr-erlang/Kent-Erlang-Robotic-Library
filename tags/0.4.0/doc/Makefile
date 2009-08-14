doc:
	$(MAKE) -C cdoc
	$(MAKE) -C edoc

upload:
	scp -r cdoc edoc matx3001,kerl@web.sourceforge.net:htdocs/

clean:
	$(MAKE) -C cdoc clean
	$(MAKE) -C edoc clean

