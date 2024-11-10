test:
	clojure -X:test

uberjar:
	clojure -M -e "(compile 'remplater.cli)"
	clojure -M:uberjar --main-class remplater.cli
