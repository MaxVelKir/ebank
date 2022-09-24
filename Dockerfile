ARG ALPINE_VERSION=3.16.0
ARG ERLANG_VERSION=25.0
ARG ELIXIR_VERSION=1.14.0
ARG MIX_ENV

# Builder Stage
#
# This builder stage is similar to a development environment. All tools and
# compile-time dependencies are packed here.
# 

FROM hexpm/elixir:$ELIXIR_VERSION-erlang-$ERLANG_VERSION-alpine-$ALPINE_VERSION as build

ENV MIX_ENV=${MIX_ENV:-prod}

WORKDIR /opt/source

RUN mix local.hex --force \
  && mix local.rebar --force \
  && apk add --no-cache 'nodejs=~16' \
  && apk add --no-cache 'npm=~8'

COPY config config
COPY mix.exs mix.lock ./
COPY apps/ebank_web/assets apps/ebank_web/assets
COPY apps/ebank_web/mix.exs apps/ebank_web/mix.exs

COPY apps/ebank/src apps/ebank/src
COPY apps/ebank/config apps/ebank/config
COPY apps/ebank/include apps/ebank/include
COPY apps/ebank/priv apps/ebank/priv
COPY apps/ebank/rebar.config apps/ebank/rebar.config

RUN mix setup \
  && mix deps.compile

COPY rel rel
COPY apps/ebank_web/assets apps/ebank_web/assets
COPY apps/ebank_web/lib apps/ebank_web/lib
COPY apps/ebank_web/priv apps/ebank_web/priv

RUN mix assets.deploy \
  && mix release ebank_web --path out

FROM alpine:$ALPINE_VERSION AS default

EXPOSE 4000

ENV SECRET_KEY_BASE=gADa4/M4bkVxKK2morfrOpPnuMngedfWUME40uIfZEmufDTGycblcPEwq74R8l4b

WORKDIR /opt

RUN apk add --no-cache 'ncurses=~6' \
  && apk add --no-cache 'openssl=~1.1' \
  && apk add --no-cache 'libstdc++'

COPY --from=build /opt/source/out/ ./

CMD ["start"]
ENTRYPOINT ["bin/ebank_web"]
