FROM haskell:8.6

RUN apt-get update && apt-get install -y \
  default-mysql-client \
  default-libmysqlclient-dev \
  libssl-dev \
  libpcre3-dev

RUN adduser --disabled-password app

RUN mkdir /home/app/.stack && chown app:app /home/app/.stack

USER app

CMD ["/bin/bash"]
