ARG ALPINE_VERSION=3.14.2
ARG ERLANG_VERSION=25.0
# Builder Stage
#
# This builder stage is similar to a development environment. All tools and
# compile-time dependencies are packed here.
#
FROM hexpm/erlang:$ERLANG_VERSION-alpine-$ALPINE_VERSION as builder
