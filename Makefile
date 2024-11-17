.PHONY: test

JAR_PATH = target/remplater.jar

test:
	clojure -X:test

uberjar:
	clojure -M -e "(compile 'remplater.cli)"
	clojure -M:uberjar --main-class remplater.cli --target $(JAR_PATH)

example-templates:
	java -jar $(JAR_PATH) generate remarkable-calendar --start-date=2024-01-01 --end-date=2025-01-31 --filename target/remarkable_calendar_2024.pdf --timeline-labels=11:12
	java -jar $(JAR_PATH) generate remarkable-calendar --start-date=2025-01-01 --end-date=2026-01-31 --filename target/remarkable_calendar_2025.pdf --timeline-labels=11:12
	java -jar $(JAR_PATH) generate remarkable-calendar --start-date=2026-01-01 --end-date=2027-01-31 --filename target/remarkable_calendar_2026.pdf --timeline-labels=11:12

	java -jar $(JAR_PATH) generate alpha --start-date=2024-11-01 --end-date=2024-12-31 --filename target/alpha_2024.pdf
