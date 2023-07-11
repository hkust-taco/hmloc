FROM sbtscala/scala-sbt:17.0.2_1.6.2_3.1.3
WORKDIR /home/mlscript
COPY . .
CMD /bin/bash
