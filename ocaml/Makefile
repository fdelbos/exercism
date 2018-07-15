all:
	@for a in $$(ls); do \
	  if [ -d $$a ]; then \
	    echo "processing folder $$a"; \
	    $(MAKE) -C $$a; \
	  fi; \
	done;
	@echo "Done!"

clean:
	@for a in $$(ls); do \
	  if [ -d $$a ]; then \
	    $(MAKE) clean -C $$a; \
	  fi; \
	done;
