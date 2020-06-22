FROM debian:9

RUN apt-get update && apt-get install -y gcc build-essential tcsh libc6 libncurses5 libncurses5-dev \
    libstdc++6 libxext6 libxext-dev libx11-6 libx11-dev libxt-dev libmotif-dev xterm wget espeak

WORKDIR /build

RUN wget -O - https://www.cs.bham.ac.uk/research/projects/poplog/V16/getpoplog.sh | bash -s -- -nopie

ENTRYPOINT ["/build/poplog_base/pop/pop/corepop"]