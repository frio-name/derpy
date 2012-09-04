setup:
	mkdir build
	mkdir -p dist/build/static/{css,js,img}

static:
	cp -a src/static build/
	cd build/static/less && \
		lessc style.less > style.css
	rm -rf dist/build/static
	mkdir -p dist/build/static/{css,js,img}
	cp -a build/static/js/* dist/build/static/js/
	cp -a build/static/less/style.css dist/build/static/css/
	cp -a build/static/img/* dist/build/static/img/

devel:
	watchr Watchfile

prod:
	cabal-dev build

refresh:
	git pull
	... 