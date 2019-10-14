FROM fedora
RUN curl -o /etc/yum.repos.d/bintray-reznikmm-matreshka.repo \
 https://bintray.com/reznikmm/matreshka/rpm && \
 dnf --assumeyes install make && \
 dnf --assumeyes install rpmdevtools && \
 dnf --assumeyes install gcc-gnat && \
 dnf --assumeyes install gprbuild && \
 dnf --assumeyes install gdb && \
 dnf --assumeyes install git && \
 dnf --assumeyes install openssh-server && \
 dnf --assumeyes install tar && \
 dnf --assumeyes install gzip && \
 dnf --assumeyes install ca-certificates && \
 dnf --assumeyes install --repo bintray--reznikmm-matreshka matreshka-devel && \
 dnf --assumeyes install --repo bintray--reznikmm-matreshka anagram-devel