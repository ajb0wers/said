# Build stage 0
FROM erlang:27-alpine

#Set working directory
RUN mkdir /data
WORKDIR /data

# COPY rebar dependencies
COPY rebar.config rebar.lock .
RUN rebar3 compile

#Copy src & config  dirs
COPY apps apps/
COPY config config/
#Build the release
RUN rebar3 as prod release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache libgcc

# Install the released application
COPY --from=0 /data/_build/prod/rel/said /said

# Expose relevant ports
EXPOSE 8080

CMD ["/said/bin/said", "foreground"]
