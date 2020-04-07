FROM debian:stretch

RUN apt-get update && apt-get install -y \
  libmariadbclient18 \
  libssl1.1 \
  libpcre3 \
  libgmp10

COPY dist/mypart-exe /usr/local/bin/mypart-exe

CMD ["/usr/local/bin/mypart-exe"]
