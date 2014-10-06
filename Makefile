all: .try_buh_touched

./dist/build/buh/buh: bin/buh.hs
	docker run \
          -v `pwd`:/source -t -i \
          images.reesd.com/reesd/stack sh -c \
          'cd /source ; cabal install'

.try_buh_touched: ./dist/build/buh/buh
	docker build -t images.reesd.com/reesd/try-buh .
	touch .try_buh_touched

run-bup:
	docker run -v `pwd`:/source -t -i \
          images.reesd.com/reesd/try-buh sh \
          /source/bup-script.sh

run-buh: ./dist/build/buh/buh
	./buh-script.sh

.PHONY: run-bup run-buh
