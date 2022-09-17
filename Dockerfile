ARG ALPINE_VERSION=3.16.0
ARG ERLANG_VERSION=25.0
# Builder Stage
#
# This builder stage is similar to a development environment. All tools and
# compile-time dependencies are packed here. You should be able to use this
# stage to run a developer workflow in Docker using:
#
#     $ docker build -t my_erl_builder --target=builder .
#     $ docker run -ti --mount type=bind,source="$PWD",target=/tmp -w /tmp \
#         my_erl_builder /bin/sh
#
FROM hexpm/erlang:$ERLANG_VERSION-alpine-$ALPINE_VERSION as builder

RUN apk add --update tar curl git bash make libc-dev gcc g++ && \
    rm -rf /var/cache/apk/

RUN set -xe \
    && curl -fSL -o rebar3 "https://s3.amazonaws.com/rebar3-nightly/rebar3" \
    && chmod +x ./rebar3 \
    && ./rebar3 local install \
    && rm ./rebar3

ENV PATH "$PATH:/root/.cache/rebar3/bin"

WORKDIR /tmp

COPY src src
COPY include include
COPY priv priv
COPY config config
COPY rebar.config rebar.config

RUN rebar3 as prod release

#
# Default Stage
#
# This stage packages the monolithic release built in the previous stage with
# the minimum system requirement to run it. This is the image that is used when
# running the project with Docker.
#

FROM alpine:$ALPINE_VERSION

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    rm -rf /var/cache/apk/

WORKDIR /opt

COPY --from=builder /tmp/_build/prod/rel/ebank ./

CMD ["foreground"]
ENTRYPOINT ["bin/ebank"]
