ARG WEBR_IMAGE=ghcr.io/r-wasm/webr@sha256:2bd309d7a4ea1daed82b6fdb8e325b0de715fcd8592c5b6f3b3b88366e70cb76

FROM $WEBR_IMAGE

# rwasm invokes pak while compiling multiple package roots. Install the known
# native build requirements once in the image so those workers do not race on
# apt metadata or depend on the runtime availability of the rig repository.
RUN rm -f /etc/apt/sources.list.d/rig.list && \
    for attempt in 1 2 3; do \
      apt-get update && break; \
      if [ "$attempt" -eq 3 ]; then exit 1; fi; \
      sleep 10; \
    done && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      libhdf5-dev \
      libjpeg-dev \
      libpng-dev \
      pandoc && \
    rm -rf /var/lib/apt/lists/*

COPY code.R /code.R

ENTRYPOINT ["Rscript"]
