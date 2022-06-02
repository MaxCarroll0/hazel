HTML_DIR=_build/default/src/hazelweb/www
HTML_FILE=$(HTML_DIR)/index.html

all: dev

deps:
	opam switch import opam.export

change-deps:
	opam switch export opam.export

dev:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

watch:
	dune build @src/fmt --auto-promote src --profile dev --watch

release:
	dune build src --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

echo-html:
	@echo "$(HTML_FILE)"

win-chrome:
	"/mnt/c/Program Files (x86)/Google/Chrome/Application/chrome.exe" "$(HTML_DIR)/index.html"

win-firefox:
	"/mnt/c/Program Files/Mozilla Firefox/firefox.exe" "$(HTML_DIR)/index.html"

firefox:
	firefox "$(HTML_FILE)" &

chrome:
	chrome "$(HTML_FILE)" &

chrome-browser:
	chrome-browser "$(HTML_FILE)" &

chromium:
	chromium "$(HTML_FILE)" &

chromium-browser:
	chromium-browser "$(HTML_FILE)" &

xdg-open:
	xdg-open "$(HTML_FILE)"

open:
	open "$(HTML_FILE)"

repl:
	dune utop src/hazelcore

test:
	dune build @src/fmt --auto-promote || true
	dune runtest || true

bench:
	dune build @src/fmt --auto-promote || true
	BENCHMARKS_RUNNER=TRUE BENCH_LIB=hazelc_test \
		dune exec -- src/hazelc/test/bench/main.exe -run-without-cross-library-inlining -reduced-bootstrap -no-sexp -quota 0.5

fix-test-answers:
	dune promote || true

clean:
	dune clean

.PHONY: all deps dev release echo-html-dir echo-html win-chrome win-firefox repl clean
