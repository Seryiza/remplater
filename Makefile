test:
	clojure -X:test

uberjar:
	clojure -M -e "(compile 'remplater.cli)"
	clojure -M:uberjar --main-class remplater.cli --target target/remplater-1.0.0.jar
