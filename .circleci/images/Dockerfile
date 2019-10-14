FROM registry.fedoraproject.org/fedora-minimal:30
RUN curl -o /etc/yum.repos.d/bintray-reznikmm-matreshka.repo \
 https://bintray.com/reznikmm/matreshka/rpm && \
 microdnf install --enablerepo bintray--reznikmm-matreshka \
  make \
  rpmdevtools \
  gcc-gnat \
  gprbuild \
  gdb \
  git \
  openssh-server \
  tar \
  gzip \
  ca-certificates \
  matreshka-devel \
  anagram-devel && \
 microdnf clean all