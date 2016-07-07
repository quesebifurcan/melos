FROM clojure:lein-2.6.1
FROM python:3.4.3

WORKDIR /root

ADD http://download.linuxaudio.org/lilypond/binaries/linux-64/lilypond-2.18.2-1.linux-64.sh ./
ADD http://download.linuxaudio.org/lilypond/binaries/linux-64/lilypond-2.19.20-1.linux-64.sh ./

RUN chmod +x lilypond-2.19.20-1.linux-64.sh
RUN ./lilypond-2.19.20-1.linux-64.sh --batch --prefix /root/unstable

# lein install subproject
# python deps
ADD . /opt/melos
WORKDIR /opt/melos

CMD ["bash"]
