# Build stage 0
FROM erlang:29

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

FROM erlang:29-slim

# Install the released application
COPY --from=0 /data/_build/prod/rel/said /said

# Expose relevant ports
EXPOSE 10000

CMD ["/said/bin/said", "foreground"]
