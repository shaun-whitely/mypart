FROM haskell:8.6

RUN apt-get update && apt-get install -y \
  default-mysql-client \
  default-libmysqlclient-dev \
  libssl-dev \
  libpcre3-dev

WORKDIR /app

COPY stack.yaml /app/
COPY package.yaml /app/

RUN stack build --only-dependencies
RUN stack test --only-dependencies
