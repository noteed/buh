./dist/build/buh/buh: bin/buh.hs
	docker run \
          -v `pwd`:/source -t -i \
          images.reesd.com/reesd/stack sh -c \
          'cd /source ; cabal install'

run-bup:
	docker run -v `pwd`:/source -t -i \
          images.reesd.com/noteed/bup sh \
          /source/bup-script.sh

run-buh: ./dist/build/buh/buh
	./buh-script.sh

.PHONY: run-bup run-buh
