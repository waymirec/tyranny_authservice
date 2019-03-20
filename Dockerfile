# Build stage 0
FROM erlang:alpine

# Install some libs
RUN apk add --no-cache git && \
    apk add --no-cache ncurses-libs

# Install Rebar3
RUN mkdir -p /buildroot
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3
RUN chmod a+x /buildroot/rebar3

# Setup environment
ENV PATH=/buildroot/rebar3:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang application
COPY apps apps
COPY config config
COPY rebar.config rebar.config

# And build the release
# RUN rebar3 as prod release
RUN ["./rebar3", "as", "prod", "release"]

# Expose relevant ports
EXPOSE 5554
EXPOSE 12345

WORKDIR /buildroot
CMD ["/buildroot/rebar3","shell"]
