FROM naartjie/alpine-lein

WORKDIR /clj

# 3449 is default http and websocket port that figwheel uses to communicate
EXPOSE 3449
# 7888 is the default nrepl port
EXPOSE 7888

COPY project.clj project.clj

RUN lein deps
RUN lein with-profile dev deps

COPY . .

RUN lein cljsbuild once dev

CMD ["lein",  "figwheel"]
