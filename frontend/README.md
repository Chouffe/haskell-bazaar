# Haskell Bazaar Frontend

## Overview

![Landing Page](./doc/images/frontend-landing-page-1.png)
![Search](./doc/images/frontend-search-1.png)

## Setup

To get an interactive development environment run from the root folder of the repo:
```
docker-compose up
```
and open your browser at [localhost:8080](http://localhost:3449/)
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

```
cd frontend; make repl
dev:cljs.user=> (js/alert "Am I connected?")
```

and you should see an alert in the browser window.

## Development

The development commands are defined in the `./backend/Makefile` file. Below is a list of the most useful commands for development:

* Attach to the figwheel REPL
```
make repl
```
* Build the code for production
```
make build
```
* Clean builds
```
make clean
```
* Deploy
```
make deploy
```

## License

Copyright © 2018 Arthur Caillau

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
