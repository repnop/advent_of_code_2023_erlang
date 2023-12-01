# [Choice] alpine,...
ARG VARIANT="alpine"
ARG VERSION="25"
FROM erlang:${VERSION}-${VARIANT}

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1001
ARG GRPNAME=vscodegrp

ENV DEBIAN_FRONTEND=noninteractive

RUN apk add git
RUN git clone https://github.com/erlang/otp  /usr/local/lib/erlang-otp-source
RUN cd /usr/local/lib/erlang-otp-source && git checkout maint-25

RUN addgroup -g $USER_GID $GRPNAME \
    && adduser -u $USER_UID -g $GRPNAME -D $USERNAME
USER $USERNAME
