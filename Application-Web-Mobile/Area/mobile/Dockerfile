
FROM mobiledevops/android-sdk-image:28.0.3

ENV FLUTTER_VERSION="2.0.2"
ENV FLUTTER_HOME "/home/mobiledevops/.flutter-sdk"
ENV PATH $PATH:$FLUTTER_HOME/bin

# Download and extract Flutter SDK
RUN mkdir $FLUTTER_HOME \
    && cd $FLUTTER_HOME \
    && curl --fail --remote-time --silent --location -O https://storage.googleapis.com/flutter_infra/releases/stable/linux/flutter_linux_${FLUTTER_VERSION}-stable.tar.xz \
    && tar xf flutter_linux_${FLUTTER_VERSION}-stable.tar.xz --strip-components=1 \
    && rm flutter_linux_${FLUTTER_VERSION}-stable.tar.xz

COPY . .
RUN flutter --version
USER root
RUN chown root -R .
RUN flutter pub get
RUN flutter doctor --android-licenses
RUN flutter build apk --split-per-abi
CMD ["cp", "-r", "./build/app/outputs/apk/release/", "/workspace/static"]
