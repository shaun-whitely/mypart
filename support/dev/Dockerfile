FROM haskell:8.6

RUN apt-get update && apt-get install -y \
  default-mysql-client \
  default-libmysqlclient-dev \
  libssl-dev \
  libpcre3-dev

CMD ["/bin/bash"]
