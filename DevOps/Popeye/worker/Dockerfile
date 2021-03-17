FROM maven:3.5-jdk-8-alpine AS builder

COPY . /worker
WORKDIR /worker
RUN mvn dependency:resolve && mvn package

FROM openjdk:8-jre-alpine
COPY --from=builder . .
WORKDIR /worker/target
CMD ["java", "-jar", "worker-jar-with-dependencies.jar"]